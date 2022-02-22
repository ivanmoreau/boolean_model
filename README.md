# boolean_model (P04 -  CCOS 264 Information retrieval)

A tool for CCOS 264 (Information retrieval). :)

Usage:

Get files from https://github.com/ivanmoreau/prep_textos

```bash
ivanmolinarebolledo@Ivans-macOS boolean_model % stack run -- --help
The boleanmodel program

boleanmodel [COMMAND] ... [OPTIONS]

Common flags:
  -? --help         Display help message
  -V --version      Print version information

boleanmodel matrix [OPTIONS]

  -f --from=ITEM  
  -t --to=ITEM    

boleanmodel query [OPTIONS]

  -o --ogfile=ITEM
  -m --matrix=ITEM
  -q --query=ITEM 
ivanmolinarebolledo@Ivans-macOS boolean_model % 
```

Grammar
```bash
Query -> a
a -> o ("and" o)*
o -> term ("or" term)*
term -> "not" Query | "(" Query ")" | Identifier
Identifier -> alphaNum (alphaNum | "'")* 
```


Query examples:

```bash
ivanmolinarebolledo@Ivans-macOS boolean_model % stack run -- query -o="Tweets.txt" -m="mm.ths" -q="cama or museo"       
[13] @LuisGcortez tweeted: Parece que falta algo de atención y mantenimiento al museo de la ciudad por parte del @ICAdifusion http://t.co/FLbC5SbcoF
[16] @yosoyloagui tweeted: Tu y yo juntos en un lugar perfectollamado mi cama *.*
[201] @JaAC9510 tweeted: Hoy me niego a levantarme de mi cama

ivanmolinarebolledo@Ivans-macOS boolean_model % stack run -- query -o="Tweets.txt" -m="mm.ths" -q="(invito and not carnal) or novio" 
[42] @KarlyyRG tweeted: Lo que mas me encanta de mi novio es que me hace morir de risa.
[49] @KarlyyRG tweeted: Nada mejor que tener un novio super divertido que en lugar de limitarte, te sigue el pedo.
[82] @Fea_situ_fea tweeted: "Y si te invito a una copa y me acerco a tu boca..." ♫
[186] @Andresrgtz tweeted: Mi amiga se besó a una mujer muy sexi el fin de semana no le digan a su novio

ivanmolinarebolledo@Ivans-macOS boolean_model % stack run -- query -o="Tweets.txt" -m="mm.ths" -q="vida and riesgo or dios"         
[203] @abrilushiZ tweeted: RT @_equiswe: Si Dios me quita la vida antes que a ti...
[206] @alex_dsr01 tweeted: Ni pedo la vida es un riesgo -Fergus
[345] @CaballoNegroII tweeted: RT @MyRyCaR: Gracias por tu presencia en mi vida...un regalo de Dios 😆 teee amooo @CaballoNegroII http://t.co/grQRS43NK1

ivanmolinarebolledo@Ivans-macOS boolean_model % stack run -- query -o="Tweets.txt" -m="mm.ths" -q="tortillas or hambre or feliz"
[14] @mc_gabucha tweeted: Ahora entiendo por que se juntó la necesidad con el hambre... ¡Que gente tan acomplejada!
[27] @IAN_BLACK26 tweeted: Mitad de quincena que comiencen los juegos del hambre.
[69] @PiernasAlLomo tweeted: @La_Montserrat a las tortillas?
[109] @zubyelynl tweeted: Hola feliz domingo tengan tod@s hoy les presento esta capa o poncho que se puede usar tanto en temporada de frio... http://t.co/qh1xeinf22
[236] @nichelopezc tweeted: Hoy ni la pizza me hace feliz carajo!!
[265] @fantasmaluigui tweeted: Siempre recuerda: No tomes decisiones cuando estés enojado, y no hagas promesas cuando estés feliz.
[325] @123anotaz tweeted: RT @CarlaMorrAgs: La mujer es una obra de arte que ilumina los ojos de quien la mira. Feliz Dia de la Mujer @CarlaMorrisonmx ♥

ivanmolinarebolledo@Ivans-macOS boolean_model % stack run -- query -o="Tweets.txt" -m="mm.ths" -q="amiga and novio"
[186] @Andresrgtz tweeted: Mi amiga se besó a una mujer muy sexi el fin de semana no le digan a su novio

```