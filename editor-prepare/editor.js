
// https://blog.datacamp.engineering/codemirror-6-getting-started-7fd08f467ed2
// BRACKETS: https://stackoverflow.com/questions/70758962/how-to-configure-custom-brackets-for-markdown-for-codemirror-closebrackets
// BRACKETS: https://bl.ocks.org/curran/d8de41605fa68b627defa9906183b92f

import {EditorState,basicSetup} from "@codemirror/basic-setup"
// import {javascript} from "@codemirror/lang-javascript"

import {EditorView} from "@codemirror/view"

const fixedHeightEditor = EditorView.theme({
    "&": {height: "700px"   },
    ".cm-scroller": {overflow: "auto"}
  })

let myTheme = EditorView.theme({

  ".cm-content": {
    caretColor: "#0e9"
  },
  "&.cm-focused .cm-cursor": {
    borderLeftColor: "#0e9"
  },
  "&.cm-focused .cm-selectionBackground, ::selection": {
    backgroundColor: "#074"
  },
  "&.cm-gutters": {
    backgroundColor: "#045",
    color: "#ddd",
    border: "none"
  },
  "&.cm-matching-bracket": { background: "#f70a0a" }  // not working
//  "&.cm-editor": {
//      resize: both,
//      height: auto,
//      maxheight: "200px"
//    }




}, {dark: true})


class CodemirrorEditor extends HTMLElement {

    static get observedAttributes() { return ['selection', 'linenumber', 'text']; }

    constructor(self) {

        self = super(self)
        console.log("CM EDITOR: In constructor")

        return self
    }

    connectedCallback() {

        console.log("CM EDITOR: In connectedCallback")

            function sendText(editor) {
                const event = new CustomEvent('text-change', { 'detail': editor.state.doc.toString() , 'bubbles':true, 'composed': true});
                editor.dom.dispatchEvent(event);
             }

           // Set up editor if need be and point this.editor to it
            if (this.editor) {
                    editor = this.editor
                } else {
                    const options = {}
                    let editor = new EditorView({
                               state: EditorState.create({
                                 extensions: [basicSetup
                                   , fixedHeightEditor
                                   , myTheme
                                   , EditorView.lineWrapping
                                   // Below: send updated text from CM to Elm
                                   , EditorView.updateListener.of((v)=> {
                                       if(v.docChanged) {
                                           sendText(editor)
                                       }
                                     })
                                   ],
                               doc: "EMPTY\n1\n2\n3"

                               }),
                               parent: document.getElementById("editor-here")

                             })

                    this.dispatchEvent(new CustomEvent("editor-ready", { bubbles: true, composed: true, detail: editor }))
                    this.editor = editor
                    // this.editor.focus = editorFocus

                }
    }


    attributeChangedCallback(attr, oldVal, newVal) {

             function sendSelectedText(editor, str) {
                                         console.log("sendSelectedText (dispatch)", str)
                                         const event = new CustomEvent('selected-text', { 'detail': str , 'bubbles':true, 'composed': true});
                                         editor.dom.dispatchEvent(event);
                                      }

             function setEditorText(editor, str) {
                         console.log("replaceAllText (dispatch)")
                         const currentValue = editor.state.doc.toString();
                         const endPosition = currentValue.length;

                         editor.dispatch({
                           changes: {
                             from: 0,
                             to: endPosition,
                             insert: str}
                         })
                     }

            function attributeChangedCallback_(editor, attr, oldVal, newVal) {
               switch (attr) {

                  case "linenumber":
                           var lineNumber = parseInt(newVal) + 2
                           var loc =  editor.state.doc.line(lineNumber)
                           console.log("Attr case lineNumber", loc)
                           console.log("position", loc.from)
                           editor.dispatch({selection: {anchor: parseInt(loc.from)}})
                           editor.scrollPosIntoView(loc.from)
                        break
                  case "text":
                        // replace the editor text with text sent from Elm
                        // That text is set in property 'text' of editor_
                        console.log ("Attr case text: set the editor text to the string sent from Elm")
                        setEditorText(editor, newVal)
                        break

                  case "selection":
                       var selectionFrom = editor.state.selection.ranges[0].from
                       var selectionTo = editor.state.selection.ranges[0].to
                       var selectionSlice = editor.state.sliceDoc(selectionFrom,selectionTo )
                       console.log("Attr case selection", selectionSlice)
                       sendSelectedText(editor, selectionSlice)


                      break
             }
         } // end attributeChangedCallback_

         if (this.editor) { attributeChangedCallback_(this.editor, attr, oldVal, newVal)  }
         else { console.log("attr text", "this.editor not defined")}

         } // end attributeChangedCallback

  }

customElements.define("codemirror-editor", CodemirrorEditor); // (2)


