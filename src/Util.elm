module Util exposing (insertInList, insertInListViaTitle)

import Document exposing (Document)
import List.Extra


insertInList : a -> List a -> List a
insertInList a list =
    if List.Extra.notMember a list then
        a :: list

    else
        list


insertInListViaTitle : Document -> List Document -> List Document
insertInListViaTitle doc list =
    if List.Extra.notMember doc.title (List.map .title list) then
        doc :: list

    else
        list
