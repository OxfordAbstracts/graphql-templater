// import {EditorView, basicSetup} from "codemirror"
import { EditorView, basicSetup } from "codemirror";
import { linter } from "@codemirror/lint";
import { EditorState, Compartment } from "@codemirror/state";
let linting = new Compartment();

export const makeView =
  ({ parent, doc, onChange, lint }) =>
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
      ],
      parent,
      doc,
    });
  };

export const getViewContent = (view) => () => {
  return view.state.doc.toString();
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
          console.log('linting', lint)
          return lint;
        })
      ),
    });
  };
