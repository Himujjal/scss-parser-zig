# This is a guide to serialize and deserialize the AST

## The Nodes:

Let's first look at the AST nodes and then number them:

```toml
0 = AnPlusB
1 = Atrule 
2 = AtrulePrelude
3 = AttributeSelector
4 = Block
5 = Brackets
6 = ClassSelector
7 = Combinator
8 = Declaration
9 = DeclarationList
10 = Dimension
11 = FunctionNode
12 = Hash
13 = IdSelector
14 = Identifier
15 = MediaFeature 
16 = MediaQuery
17 = MediaQueryList
18 = Nth
19 = Number
20 = Operator
21 = parentheses
22 = Percentage
23 = SelectorList
24 = Selector
25 = PseudoClassSelector
26 = PseudoElementSelector
27 = Ratio
28 = Raw
29 = Rule
30 = Variable
31 = StringNode
32 = StyleSheet
33 = AtKeywordToken
34 = TypeSelector
35 = UnicodeRange
36 = URL
37 = Value
38 = MiscToken
39 = Comment
40 = Whitespace
41 = CDO
42 = CDC
43 = Error

```

How each node is structured?

The structure of each node is different according to the AST parsed. That being said,
the position of the nodes have the same `CSSLocation` structure.

We can use that to our advantage to serialize the position of the AST.


```zig
struct Node {
    loc: CSSLocation
    /// ..... other fields
} 
```


