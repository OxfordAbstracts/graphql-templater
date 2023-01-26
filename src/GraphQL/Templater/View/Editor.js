// import {EditorView, basicSetup} from "codemirror"
import { EditorView, basicSetup } from "codemirror";
import { linter } from "@codemirror/lint";
import { EditorState, Compartment } from "@codemirror/state";
import * as lang from "@codemirror/language";
import { autocompletion } from "@codemirror/autocomplete";

// function myCompletions(context) {
//   let word = context.matchBefore(/\w*/)
//   console.log({word, context})
//   if (word.from == word.to && !context.explicit)
//     return null

//   return {
//     from: word.from,
//     options: [
//       {label: "match", type: "keyword"},
//       {label: "hello", type: "variable", info: "(World)"},
//       {label: "magic", type: "text", apply: "⠁⭒*.✩.*⭒⠁", detail: "macro"}
//     ]
//   }
// }

let linting = new Compartment();

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
        autocomplete
          ? autocompletion({
              override: [
                (ctx) => {
                  const autoResult = autocomplete(ctx)();
                  console.log({ autoResult });
                  return autoResult;
                },
              ],
            })
          : [],
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
          return lint;
        })
      ),
    });
  };

export const explicit = (ctx) => ctx.explicit;

export const matchBeforeImpl = (rgx) => (ctx) => () => ctx.matchBefore(rgx);
