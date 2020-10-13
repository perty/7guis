module CrudBackendApi exposing (Database, Person, initDatabase, loadPersons)

import Task


type alias Person =
    { id : Int
    , firstName : String
    , lastName : String
    }


type alias Database =
    List Person


loadPersons : Database -> (Result String (List Person) -> msg) -> Cmd msg
loadPersons database msg =
    Task.perform msg <| Task.succeed (personLoader database)


personLoader : Database -> Result String (List Person)
personLoader database =
    Result.Ok
        database


initDatabase =
    [ Person 0 "Hans" "Emil"
    , Person 1 "Max" "Mustermann"
    , Person 2 "Roman" "Tisch"
    ]
