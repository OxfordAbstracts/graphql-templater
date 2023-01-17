// import {EditorView, basicSetup} from "codemirror"
import { EditorView, basicSetup } from "codemirror";

export const makeView =
  ({ parent, doc, onChange }) =>
  () => {
    return new EditorView({
      extensions: [
        basicSetup,
        EditorView.updateListener.of((update) => {
          if (update.docChanged) {
            onChange(update)();
          }
        }),
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
}