
exports.init =  async function(app) {

  console.log("I am starting codemirror-element: init"); // CHECK

  var codemirrorJs = document.createElement('script')
  codemirrorJs.type = 'text/javascript'
  codemirrorJs.src = "/js/codemirror.js"
  codemirrorJs.onload = initCodemirror

  document.head.appendChild(codemirrorJs);
  console.log("codemirror-element: I have appended codemirrorJs to document.head");  // CHECK




    function initCodemirror() {

      console.log("Initializing custom element codemirror-element (CodeMirror)"); // CHECK

           let template = document.createElement("template")
           template.innerHTML = `
               <style>
                   :host {
                       display: block;
                       width: 100%;
                   }
                   #codemirror-editor-container {
                       height: 100%;
                       margin-top: -54px;
                       border: 1px solid #e0e0e0;
                       border-radius: 4px;
                       font-family: "Inconsolata", "Monaco", "Menlo", "Ubuntu Mono", "Consolas", "source-code-pro", monospace;
                   }
                   .codemirror_placeholder {
                       font-family: "Inconsolata", "Monaco", "Menlo", "Ubuntu Mono", "Consolas", "source-code-pro", monospace !important;
                       color: #a0a0a0;
                   }
               </style>
               <div id="codemirror-editor-container"></div>
            `
      class CodemirrorEditor extends HTMLElement {



         // Fires when an instance of the element is created
      constructor(self) {
            console.log("codemirror: in constructor")
            // Polyfill caveat we need to fetch the right context
            // https://github.com/WebReflection/document-register-element/tree/master#v1-caveat
            self = super(self)
            // Creates the shadow root
            let shadowRoot = null
            if (self.attachShadow && self.getRootNode) {
                shadowRoot = self.attachShadow({ mode: "open" })
            } else {
                shadowRoot = self.createShadowRoot()
            }
            // Adds a template clone into shadow root
            let clone = document.importNode(template.content, true)
            // getElementById may not be polyfilled yet
            self.container = clone.querySelector("#codemirror-editor-container")
            shadowRoot.appendChild(clone)

            return self
        }

connectedCallback() {
                console.log("codemirror: In connectedCallback")
                let container = this.container
                let element = this
                let editor = null

                

                this._attached = true
            } // end connected callback

      }

      customElements.define('codemirror-editor', CodemirrorEditor)

    }
}

