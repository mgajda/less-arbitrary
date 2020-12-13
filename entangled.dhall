let entangled = https://raw.githubusercontent.com/entangled/entangled/v1.2.2/data/config-schema.dhall

let database = Some ".entangled/db.sqlite"

let jsonComment = entangled.Comment.Line "// "

let languages = entangled.languages #
    [ { name = "json", identifiers = ["json"], comment = jsonComment } ]


let watchList = [ "less-arbitrary.md" ]

in { entangled = entangled.Config :: { database = database
                                     , watchList = watchList
                                     , languages = languages }
   , annotate = entangled.Annotate.Standard
   , useLineDirectives = True
   }

