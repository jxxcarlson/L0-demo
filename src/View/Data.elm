module View.Data exposing (welcome)


welcome =
    """


| title
The L0 Markup Language

| makeTableOfContents


| heading 1
Welcome

This is a demo app for the markup language L0.  [i To see what
you can do, compare the left and right windows] (source
and rendered text).

[i [blue You can try out L0 now: just begin typing in the editor (left window). Your edits are saved ony in documents that
you own or share.]]


| heading 1
Examples

[b Basic formatting.]
[i Italic text]; (b) some [b bold text]; (c) [b [i bold-italic text]]; (d) [blue [b [i blue bold-italic text]]]


[b Links.] [link New York Times (external) https://nytimes.com],  [ilink "Surrealism" (doc in this app) id-xh164-mv973]



[b Images.] [image https://ichef.bbci.co.uk/news/976/cpsprodpb/4FB7/production/_116970402_a20-20sahas20barve20-20parrotbill_chavan.jpg [caption Himalayan bird]]

[b Itemized lists]

| item
Bread

| item
Milk

| item
Grape jelly


| heading 2
Math

An inline formula: $a^2 + b^2 = c^2$

A displayed formula:

$$
\\int_0^1 x^n dx = \\frac{1}{n+1}

A theorem:

| theorem
There are infinitely many primes $p \\equiv 1\\ mod\\ 4$.

| heading 2
Code

This is inline code: `a[0] = 1`.  And this is block code:

|| code
insertInList : a -> List a -> List a
insertInList a list =
    if List.Extra.notMember a list then
        a :: list
    else
        list


[vskip 50]


[ilink Language note id-wm152-fw616]


 
 """
