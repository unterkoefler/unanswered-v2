module Pagination exposing (controls, items)

import Element exposing (..)
import Element.Input as Input
import Url.Builder
import Element.Font as Font

items : { itemsPerPage : Int
        , currentPage : Int
        } -> List item -> List item
items { itemsPerPage, currentPage } =
    List.drop (currentPage * itemsPerPage)
        >> List.take itemsPerPage

controls : List (Attribute msg) -> { itemsPerPage : Int
           , currentPage : Int
           , numItems : Int
           , baseUrl : String 
           , queryParams : List Url.Builder.QueryParameter
           } -> Element msg
controls attrs { itemsPerPage, currentPage, numItems, baseUrl, queryParams } =
    let
        numPages : Int
        numPages =
            ceiling (toFloat numItems / toFloat itemsPerPage)

        currentPageInfo : String
        currentPageInfo =
            "Page " ++ String.fromInt (currentPage + 1) ++ " of " ++ String.fromInt numPages

        hasPrevious : Bool
        hasPrevious =
            currentPage > 0

        hasNext : Bool
        hasNext =
            currentPage < numPages - 1

        previousEl =
            if hasPrevious then
                link
                    (alignLeft :: attrs)
                    { label = text "Previous"
                    , url = Url.Builder.absolute 
                        [ baseUrl ] 
                        ((Url.Builder.int "page" (currentPage - 1)) :: queryParams)
                    }
            else
                Element.none

        nextEl =
            if hasNext then
                link
                    (alignRight :: attrs)
                    { label = text "Next"
                    , url = Url.Builder.absolute 
                        [ baseUrl ] 
                        ((Url.Builder.int "page" (currentPage + 1)) :: queryParams)
                    }
            else
                Element.none
    in
    row
        [ width fill, Font.size 14 ]
        [ previousEl
        , el [ centerX ] <| text currentPageInfo
        , nextEl
        ]

