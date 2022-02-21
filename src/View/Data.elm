module View.Data exposing (welcome)


welcome =
    """

| title
Welcome!



This is a demo app for the experimental markup language L0.

[i To see what
you can do with L0, compare the left and right windows (source
and rendered text)].  Clicking on text on the right brings the
corresponding text on the left into view.  For the reverse,
highlight text on the left and click the [b[blue Sync]] button,
lower left.

You can also click on the titles in [b [blue Published docs]] (right column) to see what has been written already in L0.


[i [blue Try out L0 now: just begin typing in the left window. Delete all the text if you want.  Don't worry — your edits won't be saved, since you don't own this document.]]

[vskip 40]

[b More info]

[b [ilink About  id-jj920-kx932]
| [ilink The L0 Language id-xh206-cx767]
| [ilink Examples  id-na181-oc100]
|  [ilink Using the app id-xi165-kh517]
| [ilink Physics doc id-fm164-pg867]]


[b First examples]


1. A link: [link New York Times https://nytimes.com]

2. An image
[image https://i.stack.imgur.com/Rr6Xg.jpg]

3. Some math: Pythagoras sez $a^2 + b^2 = c^2$.  In class we
learned that

$$
\\int_0^1 x^n dx = \\frac{1}{n+1}
$$

4. Some code: `a[0] := a[0] + 1`.  A block of code:

|| code
>>> for i in range(1,5):
...   print(i, i*i)
...
1 1
2 4
3 9
4 16

[vskip ]







 """
