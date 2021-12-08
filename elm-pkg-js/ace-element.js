// This implementation is based on http://juicy.github.io/juicy-ace-editor/
// and https://github.com/LostInBrittany/ace-widget

// Search: https://stackoverflow.com/questions/26555492/ace-editor-find-text-select-row-and-replace-text

exports.init = async function(app) {

    console.log("I am starting ace-element: init");
    var aceJs = document.createElement('script')
    aceJs.type = 'text/javascript'
    aceJs.src ="/js/ace.js"
    aceJs.onload = initAce
    document.head.appendChild(aceJs);
    console.log("ace-element: I have appended ace to document.head");


   function initAce() {
        console.log("ace-element: I am now running initAce()");
//        ace.config.set('basePath', 'https://cdnjs.cloudflare.com/ajax/libs/ace/1.4.13/')
//        ace.config.setModuleUrl("ace/theme/one_dark", "https://cdnjs.cloudflare.com/ajax/libs/ace/1.4.13/theme-one_dark.min.js");
//        // https://cdnjs.com/libraries/ace
//        console.log("ace-element: setModuleUrl");


//        ace.config.setModuleUrl(
//            "ace/theme/one_dark",
//            "http://ajaxorg.github.io/ace-builds/src-noconflict/theme-one_dark.js"
//        )



        let template = document.createElement("template")
        template.innerHTML = `
            <style>
                :host {
                    display: block;
                    width: 100%;
                }
                #ace-editor-container {
                    height: 100%;
                    margin-top: -54px;
                    border: 1px solid #e0e0e0;
                    border-radius: 4px;
                    font-family: "Inconsolata", "Monaco", "Menlo", "Ubuntu Mono", "Consolas", "source-code-pro", monospace;
                }
                .ace_placeholder {
                    font-family: "Inconsolata", "Monaco", "Menlo", "Ubuntu Mono", "Consolas", "source-code-pro", monospace !important;
                    color: #a0a0a0;
                }
            </style>
            <div id="ace-editor-container"></div>
        `

        // Shim Shadow DOM styles if needed
        if (window.ShadowDOMPolyfill) {
            WebComponents.ShadowCSS.shimStyling(template.content, "ace-editor")
        }

        // Fix focus issues in Safari and Firefox
        function editorFocus() {
            let _self = this
            setTimeout(() => {
                if (!_self.isFocused()) {
                    _self.textInput.focus()
                }
            })
            this.textInput.$focusScroll = "browser"
            this.textInput.focus()
        }


        console.log("Defining custom Ace element")

        // Creates an object based in the HTML Element prototype
        window.customElements.define("ace-editor", class AceEditor extends HTMLElement {
            get editorText() {
                //return this.textContent
                return this.editor.getSession().getValue()
            }

            set editorText(s) {
                console.log("Setting editor value:", s)
                this.editor.getSession().setValue(s)
            }

            // List of observed attributes
            static get observedAttributes() {
                return ["theme", "mode", "fontsize", "softtabs", "tabsize", "readonly", "placeholder",
                    "wrapmode", "min-lines", "max-lines", "line-numbers", "shadow-style", "text", "linenumber"
                    , "searchkey", "searchcount", "sendsync"]
            }




            // Fires when an instance of the element is created
            constructor(self) {
                console.log("In constructor")
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
                self.container = clone.querySelector("#ace-editor-container")
                shadowRoot.appendChild(clone)

                return self
            }

            connectedCallback() {
                console.log("In connectedCallback")
                let container = this.container
                let element = this
                let editor = null

                if (this.editor) {
                    editor = this.editor
                    editor.setTheme("ace/theme/one_dark")
                } else {
                    const options = {}
                    // Support autoresizing
                    if (this.hasAttribute("max-lines")) {
                        options.maxLines = Number(this.getAttribute("max-lines"))
                    }
                    if (this.hasAttribute("min-lines")) {
                        options.minLines = Number(this.getAttribute("min-lines"))
                    }

                    editor = ace.edit(container, options)

                    this.dispatchEvent(new CustomEvent("editor-ready", { bubbles: true, composed: true, detail: editor }))
                    this.editor = editor
                    this.editor.focus = editorFocus

                    // Inject base editor styles
                    this.injectTheme("#ace_editor\\.css")

                    // this.editor.setTheme("ace/theme/twilight");  ///

                    editor.getSession().on("change", (event) => {
                        element.dispatchEvent(new CustomEvent("change", { bubbles: true, composed: true, detail: event }))
                    })

                    /// BEGIN EXPERIMENT
                    // https://stackoverflow.com/questions/24607730/ace-editor-javascript-triggering-a-tab-press-event-for-ace-editors-event-hand
                    // https://stackoverflow.com/questions/42019836/correct-syntax-for-adding-a-command
                   // https://stackoverflow.com/questions/28043954/keydown-event-on-ace-editor

                    editor.commands.addCommand(
                       { name: "foobar",
                         bindKey: {win: "Esc", mac: "Esc"},
                         exec: function(editor) { console.log("selected:", editor.getSelectedText()); },
                       })

                    editor.commands.on('afterExec', eventData => {
                       if (eventData.command.name === 'foobar') {
                                 console.log('SELECTION: ' + editor.getSelectedText());
                                 element.dispatchEvent(new CustomEvent("selectedtext", { bubbles: true, composed: true, detail: event }))
                             }
                        });

//                    const event = new Event('sendsync');
//
//                    // Listen for the event.
//                    editor.addEventListener('sendsync', function (e) { console.log('SYNC') }, false);
//
//                    // Dispatch the event.
//                    // CRASH // editor.dispatchEvent(event);
//
//                    editor.getSession().on("sendsync    ", (event) => {
//                                            element.dispatchEvent(new CustomEvent("sendsync", { bubbles: true, composed: true, detail: event }))
//                                        })


                    // END EXPERIMENT
                }

                // Handle theme changes
                editor.renderer.addEventListener("themeLoaded", this.onThemeLoaded.bind(this))


                editor.find('needle',{
                        backwards: false,
                        wrap: true,
                        caseSensitive: true,
                        range: null,
                        wholeWord: false,
                        regExp: false,
                        skipCurrent : true
                    });

                // Initial attributes
                editor.setOption("printMargin", false)
                // editor.setOption("highlightActiveLine", true)
                editor.setTheme(this.getAttribute("theme"))
                // editor.setTheme('ace/theme/tomorrow_night')
                editor.setFontSize(Number(this.getAttribute("fontsize")) || 16)
                editor.setReadOnly(this.hasAttribute("readonly"))
                let session = editor.getSession()
                session.setMode(this.getAttribute("mode"))
                session.setUseSoftTabs(this.getAttribute("softtabs"))
                if (this.getAttribute("tabsize")) {
                    session.setTabSize(this.getAttribute("tabsize"))
                }
                session.setUseWrapMode(this.hasAttribute("wrapmode"))
                if (this.getAttribute("line-numbers")) {
                    editor.setOption("showLineNumbers", this.getAttribute("line-numbers") == "true")
                }
                if (this.hasAttribute("placeholder")) {
                    editor.setOption("placeholder", this.getAttribute("placeholder"))
                }

                // non-Ace specific
                if (this.hasAttribute("shadow-style")) {
                    this.injectTheme(this.getAttribute("shadow-style"))
                }
                if (this.hasAttribute("text")) {
                    this.editor.getSession().setValue(this.getAttribute("text"))
                }

                if (this.hasAttribute("focus"))
                    this.editor.focus()
                else
                    ;

                this.resizeObserver = new ResizeObserver(entries => {
                    editor.resize()
                })
                this.resizeObserver.observe(container)

                this._attached = true
            }

            disconnectedCallback() {
                this.resizeObserver.unobserve(this.container)
                this._attached = false
            }

            attributeChangedCallback(attr, oldVal, newVal) {
                if (!this._attached) {
                    return false
                }
                switch (attr) {
                    case "linenumber":
                       this.editor.scrollToLine(newVal, true, true, function () {});
                       this.editor.gotoLine(newVal, 0, true);
                       break
                    case "searchkey":
//                       var selected = this.editor.getSelectedText()
//                       var key = ""
//                       if (selected == "") { key = newVal } else { key = selected }
                       this.editor.$search.set({ needle: newVal });
                       this.editor.found = this.editor.$search.findAll(this.editor.getSession())
                       this.editor.searchIndex = 0
                       if (this.editor.found[0] != null) {
                               var  line =  this.editor.found[0].start.row + 1
                               console.log("line", line)
                               this.editor.scrollToLine(line, true, true, function () {});
                               this.editor.gotoLine(line, 0, true);
                         }


                       break
                    case "searchcount":
                       console.log(this.editor.getSelectedText())
                       console.log("searchcount", newVal)
                       if (this.editor.found != null) {
                             this.editor.searchIndex = (this.editor.searchIndex + 1) % this.editor.found.length
                             var  line2 =  this.editor.found[this.editor.searchIndex].start.row + 1
                             console.log("line2", line2)
                             this.editor.scrollToLine(line2, true, true, function () {});
                             this.editor.gotoLine(line2, 0, true);

                         }
                       break
                    case "sendsync":
                        console.log(this.editor.getSelectedText())
                    case "theme":
                        this.editor.setTheme(newVal)
                        break
                    case "mode":
                        this.editor.getSession().setMode(newVal)
                        break
                    case "fontsize":
                        this.editor.setFontSize(newVal)
                        break
                    case "softtabs":
                        this.editor.getSession().setUseSoftTabs(newVal)
                        break
                    case "tabsize":
                        this.editor.getSession().setTabSize(newVal)
                        break
                    case "readonly":
                        this.editor.setReadOnly(newVal === "" || newVal)
                        break
                    case "wrapmode":
                        this.editor.getSession().setUseWrapMode(newVal !== null)
                        break
                    case "max-lines":
                        this.editor.renderer.$maxLines = Number(newVal)
                        break
                    case "min-lines":
                        this.editor.renderer.$minLines = Number(newVal)
                        break
                    case "line-numbers":
                        this.editor.setOption("showLineNumbers", newVal)
                        break
                    case "placeholder":
                        if (newVal !== null) {
                            editor.setOption("placeholder", newVal)
                        }
                        break
                    // non-Ace specific
                    case "shadow-style":
                        if (oldVal) {
                            this.shadowRoot.querySelector(oldVal).remove()
                        }
                        this.injectTheme(newVal)
                        break
                    case "text":
                        if (!this.editor.isFocused()) {
                            this.editor.getSession().setValue(newVal)
                        }
                        break
                }
            }

            onThemeLoaded(e) {
                var themeId = "#" + e.theme.cssClass
                this.injectTheme(themeId)
                // Work around Chrome-stable bug, force repaint
                this.container.style.display = "none"
                this.container.offsetHeight
                this.container.style.display = ""
            }

            /**
                * Injects a style element into ace-editor"s shadow root
                * @param {CSSSelector} selector for an element in the same shadow tree or document as `ace-editor`
                */
            injectTheme(selector) {
                const lightStyle = this.getRootNode().querySelector(selector) || document.querySelector(selector)
                if (lightStyle !== null) {
                    this.shadowRoot.appendChild(lightStyle.cloneNode(true))
                }
            }
        })

        }

}
