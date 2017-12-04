GNATdoc.Documentation = {
  "label": "Bullfrog.Containers.Circular_Buffer.Consumer",
  "qualifier": "(nested)",
  "summary": [
  ],
  "description": [
    {
      "kind": "paragraph",
      "children": [
        {
          "kind": "span",
          "text": "Package used to retrieve items from a buffer.\n"
        }
      ]
    }
  ],
  "entities": [
    {
      "entities": [
        {
          "label": "Get",
          "qualifier": "",
          "line": 89,
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
                  "number": 89,
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
                      "text": "  "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Get",
                      "href": "docs/bullfrog__containers__circular_buffer___consumer___spec.html#L89C17"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Source",
                      "href": "docs/bullfrog__containers__circular_buffer___consumer___spec.html#L89C21"
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
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
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
                      "text": "Item_Type"
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
                  "text": "Returns the oldest item from the buffer.  Raises Container_Empty if\n"
                },
                {
                  "kind": "span",
                  "text": "there are no items to return.  Note that the buffer's copy of the item\n"
                },
                {
                  "kind": "span",
                  "text": "may only calls Finalize when the buffer is leaves scope or a new item\n"
                },
                {
                  "kind": "span",
                  "text": "is added to the same physical location as the one retrieved.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Source",
              "line": 89,
              "column": 21,
              "type": {
                "label": "Bullfrog.Containers.Circular_Buffer.Buffer",
                "docHref": "docs/bullfrog__containers__circular_buffer___spec.html#L57C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Get",
          "qualifier": "",
          "line": 96,
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
                  "number": 96,
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
                      "text": "  "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Get",
                      "href": "docs/bullfrog__containers__circular_buffer___consumer___spec.html#L96C17"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 97,
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
                      "text": "Source",
                      "href": "docs/bullfrog__containers__circular_buffer___consumer___spec.html#L97C11"
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
                  "number": 98,
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
                      "href": "docs/bullfrog__containers__circular_buffer___consumer___spec.html#L98C11"
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
                      "text": "    "
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
                  "number": 99,
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
                  "text": "Returns the oldest item from the buffer.  Returns False if there are\n"
                },
                {
                  "kind": "span",
                  "text": "no items to return and True if successful.  Note that the buffer's\n"
                },
                {
                  "kind": "span",
                  "text": "copy of the item may only call Finalize when the buffer is leaves\n"
                },
                {
                  "kind": "span",
                  "text": "scope or a new item is added to the same physical location as the one\n"
                },
                {
                  "kind": "span",
                  "text": "retrieved.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Source",
              "line": 97,
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
              "line": 98,
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
          "label": "Get",
          "qualifier": "",
          "line": 105,
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
                  "number": 105,
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
                      "text": "Get",
                      "href": "docs/bullfrog__containers__circular_buffer___consumer___spec.html#L105C17"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Source",
                      "href": "docs/bullfrog__containers__circular_buffer___consumer___spec.html#L105C21"
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
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Value",
                      "href": "docs/bullfrog__containers__circular_buffer___consumer___spec.html#L105C45"
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
                  "text": "Returns the oldest item from the buffer.  Raises Container_Empty if\n"
                },
                {
                  "kind": "span",
                  "text": "there are no items to return.  Note that the buffer's copy of the item\n"
                },
                {
                  "kind": "span",
                  "text": "may only calls Finalize when the buffer is leaves scope or a new item\n"
                },
                {
                  "kind": "span",
                  "text": "is added to the same physical location as the one retrieved.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Source",
              "line": 105,
              "column": 21,
              "type": {
                "label": "Bullfrog.Containers.Circular_Buffer.Buffer",
                "docHref": "docs/bullfrog__containers__circular_buffer___spec.html#L57C9"
              },
              "description": [
              ]
            },
            {
              "label": "Value",
              "line": 105,
              "column": 45,
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
          "label": "Reset",
          "qualifier": "",
          "line": 110,
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
                  "number": 110,
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
                      "text": "Reset",
                      "href": "docs/bullfrog__containers__circular_buffer___consumer___spec.html#L110C17"
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
                      "href": "docs/bullfrog__containers__circular_buffer___consumer___spec.html#L110C23"
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
                  "text": "Resets to buffer to be empty.  Note that the buffer's copy of the\n"
                },
                {
                  "kind": "span",
                  "text": "items may only call Finalize when the buffer is leaves scope or new\n"
                },
                {
                  "kind": "span",
                  "text": "items are added to the same physical location as the ones retrieved.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Target",
              "line": 110,
              "column": 23,
              "type": {
                "label": "Bullfrog.Containers.Circular_Buffer.Buffer",
                "docHref": "docs/bullfrog__containers__circular_buffer___spec.html#L57C9"
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