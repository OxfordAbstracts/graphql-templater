import { EditorView, basicSetup } from "codemirror";
import { linter } from "@codemirror/lint";
import { EditorState, Compartment } from "@codemirror/state";
import * as lang from "@codemirror/language";
import { autocompletion } from "@codemirror/autocomplete";

let linting = new Compartment();
let autocompleteCompartment = new Compartment();

export const makeView =
  ({ parent, doc, onChange, lint, autocomplete }) =>
  () => {
    return new EditorView({
      extensions: [
        basicSetup,
        EditorView.updateListener.of((update) => {
          if (update.docChanged) {
            onChange(update)();
          }
        }),
        linting.of(
          linter((view) => {
            return lint;
          })
        ),
        autocompleteCompartment.of(
          autocomplete
            ? autocompletion({
                override: [
                  (ctx) => {
                    return autocomplete(ctx)();
                  },
                ],
              })
            : []
        ),
      ],
      parent,
      doc,
    });
  };

export const getViewContent = (view) => () => {
  return view.state.doc.toString();
};

export const setContent = (content) => (view) => () => {
  const transaction = view.state.update({
    changes: { from: 0, to: view.state.doc.length, insert: content },
  });
  view.dispatch(transaction);
};

export const getViewUpdateContent = (viewUpdate) => () => {
  return viewUpdate.state.doc.toString();
};

export const relintImpl =
  ({ view, lint }) =>
  () => {
    view.dispatch({
      effects: linting.reconfigure(
        linter((view) => {
          return lint;
        })
      ),
    });
  };

export const reloadAutocompleteImpl =
  ({ view, autocomplete }) =>
  () => {
    view.dispatch({
      effects: autocompleteCompartment.reconfigure(
        autocomplete
          ? autocompletion({
              override: [
                (ctx) => {
                  return autocomplete(ctx)();
                },
              ],
            })
          : []
      ),
    });
  };

export const explicit = (ctx) => ctx.explicit;

export const matchBeforeImpl = (rgx) => (ctx) => () => ctx.matchBefore(rgx);
