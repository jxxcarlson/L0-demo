module View.Data exposing (welcome)


welcome =
    """
| title
Welcome!


This is a demo app for the markup language L0.  Examples: (a) some
[i italic text]; (b) some [b bold text].

Things enclosed
in brackets are called [i expressions].  Expressions can be nested,
e.g., to make [b [i bold-italic text]]. 

[i [b [blue You can try out L0 now: just begin typing in this window.  Don't worry!  Your edits will not be saved.]]]

[i [blue To see what you can do, compare the what is in the editor (left window) with the rendered text (right window).]]


[b More Examples]

You can make links: [link New York Times https://nytimes.com]

As well as links to public docs on this site: [ilink Surrealism id-xh164-mv973]

Also images: [image https://ichef.bbci.co.uk/news/976/cpsprodpb/4FB7/production/_116970402_a20-20sahas20barve20-20parrotbill_chavan.jpg [caption Himalayan bird]]

Itemized lists:

| item
Bread

| item
Milk

| item
Grape jelly


[b Math]

An inline formula: $a^2 + b^2 = c^2$

A displayed formula: 

$$
\\int_0^1 x^n dx = \\frac{1}{n+1}

A theorem:

| theorem
There are infinitely many primes $p \\equiv 1\\ mod\\ 4$.



[vskip]


[b Language Note]

L0 is made of just two constructs: expressions and blocks

Expressions have the form

|| code
[Name Body]

where the body is an expression. Here are the rules for forming
expressions:

| numbered
ordinary text is an expression

| numbered
`[Name Body]` is an expression.  Here `Name` is the name of
the expression, e.g., `i`, `b`, `link`, `image`, and `Body` is any
expression, e.g., plain text.

| numbered
A seqeunce of experessions separated by spaces is an expression


Ordinary blocks have the form

|| code
| block-name
expression

Verbatim blocks have the form

|| code
|| block-name
"""


welcome2 =
    """

 | title
 Welcome!

| makeTableOfContents


| heading 1
The L0 Markup Language

L0 is a simple yet expressive markup language. Use this app
to experiment with it.  For now, look at the examples
below.  We will have a language manual for 
you soon.  The app is pre-release software.

| heading 1
Marked-up Text

Here is some L0 text:

|| code
This is [italic italic text].  
This is [blue [i blue italic text.]]
This is a link: [link New York Times http://nytimes.com].
 
For images we say [image URL] or 
[imageg URL [caption CAPTION]]

It renders as follows:

This is [italic text].  
This is [blue [i blue italic text]].
This is a link: [link New York Times http://nytimes.com].


[image https://ichef.bbci.co.uk/news/976/cpsprodpb/4FB7/production/_116970402_a20-20sahas20barve20-20parrotbill_chavan.jpg [caption Himalyan bird]]


| heading 2
Expressions

Marked-up text is an [i expression], where

| item
ordinary text  is an expression

| item 
things of the form `[function-name EXPRESSION]` are expressions.

| item
a sequence of expressions separated by spaces is an expression

According to these rules, `[i [b italic bold text]]` is an 
expression.  In addition, there are two additional expressions,
one for inline code and one for inline math, e.g.

|| code
A code expression: `a[0] := 1`; 
and a math expression: $a^n \\equiv 1$.

These render as

| indent
A code expression: `a[0] := 1`; and a math expression: $a^n \\equiv 1$.


| heading 2
Blocks

L0 also has a notion of block.  Here is a code block:

|| code
|| code
a[0] := 0;
b[0]: = 0

This block renders as

|| code
a[0] := 0;
b[0]: = 0

Here is another kind of block:

|| code
| theorem
There are infinitely manty primes $p \\equiv 1\\ mod\\ 4$.


This block renders as

| theorem
There are infinitely manty primes $p \\equiv 1\\ mod\\ 4$.

An ordinary block has the form

|| code
| HEADER
BODY

while verbatim blocks have the form

|| code
|| HEADER
BODY

Both the header and the body are expressions.



| heading 2
Math

Use TeX/LaTeX notation to do math:

Pythagoras said that $a^2 + b^2 = c^2$.  In high school,
we learned that

$$
\\int_0^1 x^n dx = \\frac{1}{n+1}
$$

More generally, 

|| equation
\\int_0^a x^n dx = \\frac{a^{n+1}}{n+1}
 
 
 """
