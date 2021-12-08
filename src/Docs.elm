module Docs exposing (deleted, docsNotFound, notSignedIn)

import Document exposing (Document, empty)


notSignedIn : Document
notSignedIn =
    { empty
        | content = welcomeText
        , id = "id-sys-1"
        , publicId = "public-sys-1"
    }


deleted : Document
deleted =
    { empty
        | content = deletedText
        , id = "id-sys-2"
        , publicId = "public-sys-2"
    }


deletedText =
    """
| title
Document deleted

Your document has been deleted.


"""


welcomeText =
    """
 | title
 Welcome to the L0 Lab Demo
 
 [image https://ichef.bbci.co.uk/news/976/cpsprodpb/4FB7/production/_116970402_a20-20sahas20barve20-20parrotbill_chavan.jpg]
 
 """


docsNotFound =
    { empty
        | content = docsNotFoundText
        , id = "id-sys-2"
        , publicId = "public-sys-2"
    }


docsNotFoundText =
    """
[title Oops!]

[i  Sorry, could not find your documents]

[i To create a document, press the [b New] button above, on left.]
"""
