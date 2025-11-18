# Recovery Policy Guide (SyntaxKind-specific)

When building the Yul parser, apply error recovery at nodes that provide natural synchronization points so parsing can continue after encountering invalid tokens. The table below lists each `SyntaxKind` that benefits from a recovery policy and the delimiters you should synchronize on.

| SyntaxKind           | Recommended Sync Points                             | Notes                                                                                   |
|----------------------|-----------------------------------------------------|-----------------------------------------------------------------------------------------|
| `Assignment`         | `;`, `}`                                            | Same as above                                                                           |
| `IfStatement`        | Closing `}` of the body (and optional `else`)       | Keeps the enclosing scope intact even if the body fails                                 |
| `ForStatement`       | Closing `}` of the loop body                        | Skip to loop end                                                                        |
| `SwitchStatement`    | Closing `}` of the switch block                     | Aligns with the next statement                                                          |
| `FunctionDefinition` | Closing `}` of the function body                    | Function bodies are self-contained                                                      |

For leaf nodes (identifiers, literals, `PathSegment`, etc.), skip custom recovery—propagate errors upward so the nearest statement/block can apply its recovery strategy using the delimiters above.
