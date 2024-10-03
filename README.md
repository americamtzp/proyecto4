# Tarea: Explorando el Framework Cowboy en Erlang

## **Descripción**

En este proyecto se realizo una aplicacione web basica desarrollada en Erlang haciendo uso del framework Cowboy. 
---
## **Instalación de rebar3** 

1. **Instalar rebar3:**
```bash
wget https://github.com/erlang/rebar3/releases/download/3.20.0/rebar3
chmod +x rebar3
sudo mv rebar3 /usr/local/bin
```
2. **Revisar instalacion:**
```bash
rebar3 --version
``` 
## **Instalación del proyecto** 

1. **clonar el repositorio:**
```bash
git clone https://github.com/tu_usuario/proyecto_cowboy.git
cd mi_proyecto
```
2. **Instalar las dependencias del proyecto Cowboy:**
```bash
rebar3 get-deps
```
3. **Compila el proyecto:**
```bash
rebar3 compile
```
4. **Ejecutar el servidor**
```bash
rebar3 shell
```

## **Creacion del proyecto**

1. **Agregar Cowboy como dependencia**

Se edito el archivo rebar.config que se creo a la hora de abrir el proyecto para agregar la dependencia. 
```erlang
{deps, [
    {cowboy, "2.9.0"}
]}.

```
2. **Crear el servidor HTTP básico con Cowboy**
Crear un archivo src/proyecto_cowboy_handler.erl, el cual sera el manejador que se encargara de responder "Hola mundo"
cuando se haga una solicitud GET a la ruta.
```erlang
-module(proyecto_cowboy_handler).
-export([init/2]).
-import(cowboy_req, [reply/4]).

init(Req, State) ->
    Body = <<"¡Hola, Mundo!">>,
    Req2 = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, Body, Req),
    {ok, Req2, State}.
```
Ahora se crea el manejador para la ruta /saludo.
```erlang
-module(saludo_handler).
-export([init/2]).
-import(cowboy_req, [reply/4, parse_qs/1]).

init(Req, State) ->
    Qs = cowboy_req:parse_qs(Req),
    Nombre = case lists:keyfind(<<"nombre">>, 1, Qs) of
        false -> <<"desconocido">>;
        {_, Value} -> Value
    end,
    Body = <<"¡Hola, ", Nombre/binary, "!">>,
    Req2 = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, Body, Req),
    {ok, Req2, State}.

```
3. **Configurar el servicio**
Se ajusto la configuracion para que Cowboy utilice los manejadores que se acaban de crear, se modifican src/proyecto_cowboy_app.erl
```erlang
-module(saludo_handler).
-export([init/2]).
-import(cowboy_req, [reply/4, parse_qs/1]).

init(Req, State) ->
    Qs = cowboy_req:parse_qs(Req),
    Nombre = case lists:keyfind(<<"nombre">>, 1, Qs) of
        false -> <<"desconocido">>;
        {_, Value} -> Value
    end,
    Body = <<"¡Hola, ", Nombre/binary, "!">>,
    Req2 = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, Body, Req),
    {ok, Req2, State}.

```
