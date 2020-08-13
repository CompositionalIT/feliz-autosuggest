module Server

open Giraffe
open Saturn

open Shared
open Bogus

let faker = Faker("de")

let tenants =
    [ for _ in 1 .. 25000 -> faker.Company.CompanyName() ]
    |> List.sort
    |> List.distinct

let webApp =
    router {
        get Route.hello (json "Hello from SAFE!")
        get "/api/tenant" (json tenants)
    }

let app =
    application {
        url "http://0.0.0.0:8085"
        use_router webApp
        memory_cache
        use_static "public"
        use_json_serializer (Thoth.Json.Giraffe.ThothSerializer())
        use_gzip
    }

run app