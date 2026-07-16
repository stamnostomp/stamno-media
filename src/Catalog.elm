module Catalog exposing
    ( Genre(..)
    , MediaType(..)
    , Title
    , byGenre
    , catalog
    , find
    , genreLabel
    , initial
    , movies
    , related
    , shows
    )

{-| Static media catalog and pure filtering helpers.
Stand-in for a Jellyfin/Plex-style metadata API.
-}


type MediaType
    = Movie
    | Show


type Genre
    = Action
    | Scifi
    | Drama
    | Thriller


type alias Title =
    { id : Int
    , title : String
    , mediaType : MediaType
    , year : Int
    , duration : String
    , genre : Genre
    , rating : String
    , progress : Int
    , synopsis : String
    , cast : String
    }


genreLabel : Genre -> String
genreLabel genre =
    case genre of
        Action ->
            "ACTION"

        Scifi ->
            "SCIFI"

        Drama ->
            "DRAMA"

        Thriller ->
            "THRILLER"


initial : Title -> String
initial title =
    String.left 1 title.title


find : Int -> Title
find id =
    List.filter (\t -> t.id == id) catalog
        |> List.head
        |> Maybe.withDefault fallback


{-| Nothing means the ALL filter. -}
byGenre : Maybe Genre -> List Title -> List Title
byGenre filter titles =
    case filter of
        Nothing ->
            titles

        Just genre ->
            List.filter (\t -> t.genre == genre) titles


movies : List Title -> List Title
movies =
    List.filter (\t -> t.mediaType == Movie)


shows : List Title -> List Title
shows =
    List.filter (\t -> t.mediaType == Show)


related : Int -> List Title
related activeId =
    List.filter (\t -> t.id /= activeId) catalog
        |> List.take 6


fallback : Title
fallback =
    { id = 1
    , title = "ORBITAL DECAY"
    , mediaType = Movie
    , year = 2024
    , duration = "2h 04m"
    , genre = Scifi
    , rating = "PG-13"
    , progress = 62
    , synopsis = "A salvage crew strips a dead station for parts and finds it is not as dead as the manifest says."
    , cast = "R. Voss, M. Ilic, T. Auer"
    }


catalog : List Title
catalog =
    [ fallback
    , { id = 2
      , title = "NIGHT DRIVE"
      , mediaType = Movie
      , year = 2022
      , duration = "1h 47m"
      , genre = Thriller
      , rating = "R"
      , progress = 18
      , synopsis = "A courier takes one last job across a city that does not want her to leave it."
      , cast = "D. Marsh, K. Onwu"
      }
    , { id = 3
      , title = "SECTOR 9"
      , mediaType = Movie
      , year = 2023
      , duration = "2h 15m"
      , genre = Action
      , rating = "R"
      , progress = 0
      , synopsis = "Three ex-operatives reunite to shut down the network they used to work for."
      , cast = "J. Calder, P. Reyes, S. Ndiaye"
      }
    , { id = 4
      , title = "GLASS ORCHARD"
      , mediaType = Show
      , year = 2021
      , duration = "3 Seasons"
      , genre = Drama
      , rating = "PG-13"
      , progress = 47
      , synopsis = "A family divides an inheritance and finds out how little they knew each other."
      , cast = "A. Whitfield, L. Sorm"
      }
    , { id = 5
      , title = "LOW ORBIT"
      , mediaType = Movie
      , year = 2025
      , duration = "1h 52m"
      , genre = Scifi
      , rating = "PG-13"
      , progress = 0
      , synopsis = "The last functioning satellite relay picks up a signal it should not be able to receive."
      , cast = "N. Brandt, C. Okafor"
      }
    , { id = 6
      , title = "THE LONG SHIFT"
      , mediaType = Show
      , year = 2020
      , duration = "2 Seasons"
      , genre = Thriller
      , rating = "R"
      , progress = 0
      , synopsis = "A night-shift dispatcher realizes the calls she is routing are not going where she thinks."
      , cast = "H. Duval, R. Kessler"
      }
    , { id = 7
      , title = "RED CIRCUIT"
      , mediaType = Movie
      , year = 2019
      , duration = "2h 01m"
      , genre = Action
      , rating = "R"
      , progress = 0
      , synopsis = "A retired hacker is pulled back in when her old handle resurfaces in a ransom note."
      , cast = "V. Okonkwo, T. Reyes"
      }
    , { id = 8
      , title = "FERAL LIGHT"
      , mediaType = Show
      , year = 2023
      , duration = "1 Season"
      , genre = Drama
      , rating = "PG-13"
      , progress = 0
      , synopsis = "Two estranged brothers rebuild their father\u{2019}s workshop and, slowly, each other."
      , cast = "M. Falk, G. Amaro"
      }
    , { id = 9
      , title = "STATIC HORIZON"
      , mediaType = Show
      , year = 2024
      , duration = "2 Seasons"
      , genre = Scifi
      , rating = "PG-13"
      , progress = 0
      , synopsis = "The last transmission from a colony ship arrives eighty years after it went silent."
      , cast = "I. Solberg, Q. Marsh"
      }
    , { id = 10
      , title = "BLACKOUT LANE"
      , mediaType = Movie
      , year = 2018
      , duration = "1h 55m"
      , genre = Thriller
      , rating = "R"
      , progress = 0
      , synopsis = "A city-wide blackout strands a detective one exit from the case that ended her career."
      , cast = "F. Nakamura, D. Ellery"
      }
    , { id = 11
      , title = "IRON HARVEST"
      , mediaType = Movie
      , year = 2022
      , duration = "2h 08m"
      , genre = Action
      , rating = "R"
      , progress = 0
      , synopsis = "A convoy crew runs one final route through territory that no longer answers to anyone."
      , cast = "B. Sato, L. Kowalski"
      }
    , { id = 12
      , title = "QUIET FREQUENCY"
      , mediaType = Show
      , year = 2021
      , duration = "4 Seasons"
      , genre = Drama
      , rating = "PG-13"
      , progress = 0
      , synopsis = "A radio operator in a coastal town keeps broadcasting long after anyone is listening."
      , cast = "E. Voss, A. Petrov"
      }
    , { id = 13
      , title = "SALT FLAT"
      , mediaType = Movie
      , year = 2020
      , duration = "1h 49m"
      , genre = Action
      , rating = "R"
      , progress = 0
      , synopsis = "A getaway driver strands her crew in the desert on purpose."
      , cast = "R. Aoyama, C. Weber"
      }
    , { id = 14
      , title = "DEEP FIELD"
      , mediaType = Show
      , year = 2025
      , duration = "1 Season"
      , genre = Scifi
      , rating = "PG-13"
      , progress = 0
      , synopsis = "A telescope array picks up a pattern that repeats too perfectly to be noise."
      , cast = "P. Lindqvist, S. Okoye"
      }
    , { id = 15
      , title = "PALE HOUR"
      , mediaType = Movie
      , year = 2019
      , duration = "1h 36m"
      , genre = Drama
      , rating = "PG-13"
      , progress = 0
      , synopsis = "A night-shift nurse counts down the last hour of a job she is about to quit."
      , cast = "M. Torres, J. Lindgren"
      }
    , { id = 16
      , title = "WIRE AND ASH"
      , mediaType = Show
      , year = 2022
      , duration = "3 Seasons"
      , genre = Thriller
      , rating = "R"
      , progress = 0
      , synopsis = "An arson investigator finds her own name in the case files."
      , cast = "A. Kessler, D. Osei"
      }
    , { id = 17
      , title = "FAR SIGNAL"
      , mediaType = Movie
      , year = 2023
      , duration = "2h 06m"
      , genre = Scifi
      , rating = "PG-13"
      , progress = 0
      , synopsis = "A rescue team follows a distress call to a moon that was never charted."
      , cast = "T. Falk, N. Ibrahim"
      }
    , { id = 18
      , title = "CUT ROPE"
      , mediaType = Movie
      , year = 2021
      , duration = "1h 51m"
      , genre = Action
      , rating = "R"
      , progress = 0
      , synopsis = "A climbing guide turned smuggler runs one route too many."
      , cast = "K. Boone, R. Vidal"
      }
    ]
