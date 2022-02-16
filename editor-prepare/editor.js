
// https://blog.datacamp.engineering/codemirror-6-getting-started-7fd08f467ed2
// BRACKETS: https://stackoverflow.com/questions/70758962/how-to-configure-custom-brackets-for-markdown-for-codemirror-closebrackets
// BRACKETS: https://bl.ocks.org/curran/d8de41605fa68b627defa9906183b92f

import {EditorState,basicSetup} from "@codemirror/basic-setup"
// import {javascript} from "@codemirror/lang-javascript"

import {EditorView} from "@codemirror/view"

const fixedHeightEditor = EditorView.theme({
    "&": {height: "800px"},
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
  ".cm-gutters": {
    backgroundColor: "#045",
    color: "#ddd",
    border: "none"
  },
  ".cm-matching-bracket": { background: "#f70a0a" } // not working

}, {dark: true})


//.codemirror-matching-bracket { background: red; }
//.codemirror-nonmatching-bracket { background: green; }


class CodemirrorEditor extends HTMLElement {

    static get observedAttributes() { return ['selection', 'linenumber', 'text']; }

    get editorText() {
        //return this.textContent
        return this.editor.getSession().getValue()
    }

    set editorText(s) {
        console.log("Setting editor value:", s)
        this.editor.getSession().setValue(s)
    }

    constructor(self) {

        self = super(self)
        console.log("CM EDITOR: In constructor")
//



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
                                   , EditorView.updateListener.of((v)=> {
                                       if(v.docChanged) {
                                           sendText(editor)
                                       }
                                     })
                                   ],
                               doc: "EMPTY"

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
                                         const event = new CustomEvent('selected-text', { 'detail': str , 'bubbles':true, 'composed': true});
                                         editor.dom.dispatchEvent(event);
                                      }

             function replaceAllText(editor, str) {
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
                           editor.dispatch({selection: {anchor: parseInt(loc.from)}})
                           editor.scrollPosIntoView(loc.from)
                        break
                  case "text":
                        replaceAllText(editor, newVal)
                        break

                  case "selection":
                       var selectionFrom = editor.state.selection.ranges[0].from
                       var selectionTo = editor.state.selection.ranges[0].to
                       var selectionSlice = editor.state.sliceDoc(selectionFrom,selectionTo )
                      sendSelectedText(editor, selectionSlice)


                      break
             }
         } // end attributeChangedCallback_

         if (this.editor) { attributeChangedCallback_(this.editor, attr, oldVal, newVal)  }
         else { console.log("attr text", "this.editor not defined")}

         } // end attributeChangedCallback

  }

customElements.define("codemirror-editor", CodemirrorEditor); // (2)


