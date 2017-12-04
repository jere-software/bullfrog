GNATdoc.Documentation = {
  "label": "Bullfrog.Containers.Circular_Buffer.Producer",
  "qualifier": "(nested)",
  "summary": [
  ],
  "description": [
    {
      "kind": "paragraph",
      "children": [
        {
          "kind": "span",
          "text": "Package used to add items to a buffer\n"
        }
      ]
    }
  ],
  "entities": [
    {
      "entities": [
        {
          "label": "Put",
          "qualifier": "",
          "line": 124,
          "column": 16,
          "src": "srcs/bullfrog-containers-circular_buffer.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 124,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "function"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Put",
                      "href": "docs/bullfrog__containers__circular_buffer___producer___spec.html#L124C16"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 125,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "         "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Target",
                      "href": "docs/bullfrog__containers__circular_buffer___producer___spec.html#L125C11"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "in"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "out"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Buffer",
                      "href": "docs/bullfrog__containers__circular_buffer___spec.html#L142C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 126,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "          "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Value",
                      "href": "docs/bullfrog__containers__circular_buffer___producer___spec.html#L126C11"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "  "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "in"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "     "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Item_Type"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 127,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "          "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "return"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Boolean"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Puts a new item on the buffer.  Returns True if successful or False\n"
                },
                {
                  "kind": "span",
                  "text": "if there was no room.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Target",
              "line": 125,
              "column": 11,
              "type": {
                "label": "Bullfrog.Containers.Circular_Buffer.Buffer",
                "docHref": "docs/bullfrog__containers__circular_buffer___spec.html#L57C9"
              },
              "description": [
              ]
            },
            {
              "label": "Value",
              "line": 126,
              "column": 11,
              "type": {
                "label": "Bullfrog.Containers.Circular_Buffer.Item_Type",
                "docHref": "docs/bullfrog__containers__circular_buffer___spec.html#L38C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Put",
          "qualifier": "",
          "line": 131,
          "column": 17,
          "src": "srcs/bullfrog-containers-circular_buffer.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 131,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "procedure"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Put",
                      "href": "docs/bullfrog__containers__circular_buffer___producer___spec.html#L131C17"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 132,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "         "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Target",
                      "href": "docs/bullfrog__containers__circular_buffer___producer___spec.html#L132C11"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "in"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "out"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Buffer",
                      "href": "docs/bullfrog__containers__circular_buffer___spec.html#L142C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 133,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "          "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Value",
                      "href": "docs/bullfrog__containers__circular_buffer___producer___spec.html#L133C11"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "  "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "in"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "     "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Item_Type"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Puts a new item on the buffer.  Raises Container_Full if there is no\n"
                },
                {
                  "kind": "span",
                  "text": "room to put the item.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Target",
              "line": 132,
              "column": 11,
              "type": {
                "label": "Bullfrog.Containers.Circular_Buffer.Buffer",
                "docHref": "docs/bullfrog__containers__circular_buffer___spec.html#L57C9"
              },
              "description": [
              ]
            },
            {
              "label": "Value",
              "line": 133,
              "column": 11,
              "type": {
                "label": "Bullfrog.Containers.Circular_Buffer.Item_Type",
                "docHref": "docs/bullfrog__containers__circular_buffer___spec.html#L38C9"
              },
              "description": [
              ]
            }
          ]
        }
      ],
      "label": "Subprograms"
    }
  ]
};