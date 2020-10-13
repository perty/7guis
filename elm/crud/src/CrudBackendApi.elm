module CrudBackendApi exposing (Database, Person, createPerson, deletePerson, initDatabase, loadPersons, updatePerson)

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


createPerson : Database -> Person -> (Result String (List Person) -> msg) -> Cmd msg
createPerson database person msg =
    Task.perform msg <| Task.succeed (appendDatabase database person)


appendDatabase : List Person -> Person -> Result error (List Person)
appendDatabase database person =
    Result.Ok <| database ++ [ Person (nextIndex database) person.firstName person.lastName ]


nextIndex : List { a | id : Int } -> Int
nextIndex database =
    1 + (List.map .id database |> List.maximum |> Maybe.withDefault 0)


updatePerson : Database -> Person -> (Result String (List Person) -> msg) -> Cmd msg
updatePerson database person msg =
    Task.perform msg <| Task.succeed (updateDatabase database person)


updateDatabase : List Person -> Person -> Result error (List Person)
updateDatabase database person =
    Result.Ok (person :: List.filter (\p -> p.id /= person.id) database)


deletePerson : Database -> Person -> (Result String (List Person) -> msg) -> Cmd msg
deletePerson database person msg =
    Task.perform msg <| Task.succeed (deleteFromDatabase database person)


deleteFromDatabase : List Person -> Person -> Result error (List Person)
deleteFromDatabase database person =
    Result.Ok (List.filter (\p -> p.id /= person.id) database)


personLoader : Database -> Result String (List Person)
personLoader database =
    Result.Ok
        database


initDatabase =
    [ Person 0 "Hans" "Emil"
    , Person 1 "Max" "Mustermann"
    , Person 2 "Roman" "Tisch"
    , Person 3 "Anders" "Andersson"
    , Person 4 "Kim" "Wallner"
    , Person 5 "Lars Magnus" "Eriksson"
    , Person 6 "Doris" "Enström"
    , Person 7 "Jacob Johan" "Anckarström"
    , Person 8 "Per" "Lundholm"
    , Person 9 "Elsie" "Ottar"
    ]
