module CrudBackendApi exposing (Person, loadPersons)

import Task


type alias Person =
    { firstName : String
    , lastName : String
    }


loadPersons : (Result String (List Person) -> msg) -> Cmd msg
loadPersons msg =
    Task.perform msg <| Task.succeed personLoader


personLoader : Result String (List Person)
personLoader =
    Result.Ok
        [ Person "Hans" "Emil"
        , Person "Max" "Mustermann"
        , Person "Roman" "Tisch"
        ]
