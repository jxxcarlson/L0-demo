module View.Data exposing (welcome)


welcome =
 | title
 Welcome!

 This is a demo app for the markup language L0.

 [i To see what
 you can do with it, compare the left and right windows (source
 and rendered text)].

 [i [blue [b You can try out L0 now: just begin typing in the
 left window.  Don't worry — your edits won't be saved, since
 you don't own this document.]]]

 A few examples:


 1. A link: [link New York Times https://nytimes.com]

 2. An image
  [image https://i.stack.imgur.com/Rr6Xg.jpg]

 3. Some math: Pythagoras sez $a^2 + b^2 = c^2$.  In class we
 learned that

 $$
 \\int x^n dx = \\frac{1}{n+1}
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


 [b More info:]

 | item
 [ilink L0 Examples  id-na181-oc100]

 | item
 [ilink About the L0 Language id-wm152-fw616]

 | item
 [ilink Using this app id-xi165-kh517]




  
 """
