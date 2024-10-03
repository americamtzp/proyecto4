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
-module(proyecto_cowboy_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    % Asegurarse de que Ranch y Cowboy estén iniciados
    {ok, _} = application:ensure_all_started(ranch),
    
    % Definir el router para las rutas
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", proyecto1_handler, []},
            {"/saludo", saludo_handler, []}
        ]}
    ]),

    % Iniciar el listener HTTP en el puerto 8080
    {ok, _} = cowboy:start_clear(http_listener, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }),

    % Iniciar el supervisor de la aplicación
    proyecto_cowboy_sup:start_link().

stop(_State) ->
    ok.

```
4. **Crear el supervisor**
Despues, nos aseguramos de tener el archivo de supervisor basico src/proyecto_cowboy_sup.erl
```erlang
-module(proyecto_cowboy_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    % Definir los hijos que serán supervisados (vacío por ahora)
    Children = [],
    % Estrategia de supervisión
    {ok, {{one_for_one, 5, 10}, Children}}.
```
5. **Compilar y ejecutar**
```bash
rebar3 compile
rebar3 shell
```
## **Videos de Asciinema**

1. **Instalacion de Rebar3**

https://asciinema.org/a/j55ixeYpXjQGA15KsTIXcvXAn

2. **Pruebas de las rutas con curl**

https://asciinema.org/a/QTfTlaDq0MLNaphN5b5p8zjmx

3. **Compilacion y ejecucion del servidor**

https://asciinema.org/a/lTz7yriMDOtaKPGY6jVx4QifA

## **Captura de pantallas de las pruebas en navegador**

![image](https://github.com/user-attachments/assets/9f562fb0-d03e-40fc-b02e-e185214de32e)

![image](https://github.com/user-attachments/assets/98d02563-e9c6-42ec-9b5f-191877a94a20)

## **Ejercicios practicos adicionales**

###**Despliegue de la Aplicacion Cowboy**

Para desplegar una aplicacion web realizada con Cowboy en un servidor se hace lo siguiente: 

1.**Preparar el servidor:** Se asegura de tener un servidor Linux (Ubuntu, por ejenplo), donde puedas instalar Erlang y Cowboy. Necesitarás tener redar3 para asi poder gestionar las dependencias. 
2.**Configurar el puerto:** Cowboy, por defecto, usa el puerto 8080. Si quieres usar HTTPS (Es más seguro), utiliza el puerto 443. Cambia la configuracion en el archivo rebar.config para definir el puerto que se desea: 
```erlang
{http, [{port, 8080}]}.
```
3.**Asegurar el servidor**
Configurar el Firewall: Asegura que el servidor permita el trafico en los puertos que se han configurado, se usa el siguiente comando. 
```bash
sudo ufw allow 8080
sudo ufw allow 443
```
Habilitar HTTPS: Par proteger la apicacion, podemos usar certificados SSL. Puedes obtener certificados gratuitos con Lets Encrypt. Configura tu aplicación para usar TLS(HTTPS). Por ejemplo: 
```erlang
{ok, _} = cowboy:start_tls(my_https_listener, 100, [{port, 443}, 
    {certfile, "ruta/a/tu/certificado.pem"},
    {keyfile, "ruta/a/tu/clave.pem"}], 
    #{dispatch => dispatch_rules}).
```
4.**Monitorear la aplicacion** Se usa un supervisor para asegurarnos de que la aplicacion este siempre corriendo. Esto significa que si algo falla, el supervisor lo reiniciara automaticamente. 

5.**Desplegar la Aplicación:**
Compila la aplicacion con lo siguiente: 
```bash
rebar3 compile
```
y ejecutalo con: 
```erlang
rebar3 shell
```
6.**Probar la Aplicacion:** Después de desplegar, prueba tu aplicacion abriendo un navegador accediendo a la direccion del servidor o tambien probando en la terminal con curl. 
