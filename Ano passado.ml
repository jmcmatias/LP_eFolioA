open Scanf

(**Vaviáveis mutáveis para contadores**)
let count_linha = ref 0;; 
let risco_alto = ref 0;;
let risco_medio = ref 0;;
let risco_baixo = ref 0;;
let semavaliacao = ref 0;;

(**Funções para contadores*)
let next_linha = fun () -> count_linha := (!count_linha) + 1; print_string "Paciente N."; print_int !count_linha;; (**Alem de contar, imprime contador*)
let add_risco_alto = fun () -> risco_alto := (!risco_alto) + 1;;
let add_risco_medio = fun () -> risco_medio := (!risco_medio) + 1;;
let add_risco_baixo = fun () -> risco_baixo := (!risco_baixo) + 1;;
let add_semavaliacao = fun () -> semavaliacao := (!semavaliacao) + 1;;
        


(**Analisa os critérios e informa o utilizador do resultado da linha, acrescenta ao respectivo totalizador**)
let analisa_paciente lista =
  
  next_linha();

  (**Critério de paciente de alto risco**)
  if ( int_of_string (List.nth lista 0) > 2
       && int_of_string (List.nth lista 1) > 3
       && String.uppercase_ascii(List.nth lista 3) = "S" 
       && String.uppercase_ascii(List.nth lista 5) = "N" 
       && (String.uppercase_ascii(List.nth lista 9) = "ALTO" || String.uppercase_ascii(List.nth lista 9) = "MEDIO")
     )
    then ( add_risco_alto();
           print_string " é de risco Alto\n")

  (**Critérios de paciente de risco médio**)
  else if ( int_of_string (List.nth lista 0) < 3
            && int_of_string (List.nth lista 1) < 4
            && String.uppercase_ascii(List.nth lista 6) = "N" 
            && String.uppercase_ascii(List.nth lista 8) = "N" 
            &&  String.uppercase_ascii(List.nth lista 9) = "MEDIO"
          )
          then (add_risco_medio();
                print_string " é de risco Médio\n")
 
  (**Critérios de paciente de risco Baixo**)
  else if ( int_of_string (List.nth lista 0) < 3
          && String.uppercase_ascii(List.nth lista 2) = "N" 
          && String.uppercase_ascii(List.nth lista 3) = "N" 
          && String.uppercase_ascii(List.nth lista 6) = "N" 
          && String.uppercase_ascii(List.nth lista 9) = "BAIXO" 
        )
        then (add_risco_baixo();
              print_string " é de risco Baixo\n")
  else
    (**Não enquadra nos critérios definidos**)
    (add_semavaliacao(); print_string " não enquadra nos critérios de avaliação\n");;



(**Valida a linha utilizando uma expressão regular, tem de cumprir a estrutura pré-definida**)
let valida_linha l =
  if (Str.string_match (Str.regexp "[0-9]+[;][0-9]+[;][s|S|n|n][;][s|S|n|n][;][s|S|n|n][;][s|S|n|n][;][s|S|n|n][;][s|S|n|n][;][s|S|n|n][;][b|B|m|M|a|A][a|A|e|E|l|L][i|I|d|D|t|T][x|X|i|I|o|O][o|O]?") l 0)
    then
      analisa_paciente (Str.split (Str.regexp ";") l) (**Convert a linha numa lista de dados, utiliza a separação dos dados pelo ";" **)
    else
      print_string "Linha invalida\n" (**Informa o utilizador que a linha não está dentro do parâmetros**)
    ;;

(**Ciclo que extrai linha a linha do ficheiro**)    
let le_ficheiro ficheiro =
  let in_channel = open_in ficheiro in
    try
    while true
      do
        let linha = input_line in_channel in
        valida_linha linha (**Valida a linha que acabou de extrair**)
      done
      with End_of_file ->
          close_in in_channel;;


(**Valida o nome do ficheiro utilizando uma expressão regular**)
let valida_ficheiro f = 
  if (Str.string_match (Str.regexp "[a-z||A-Z|_]*[.][t|T][x|X][t|T]") f 0)
  then
    le_ficheiro f
  else
      print_string "Ficheiro inválido!\n";; (**Nome do ficheiro não cumpre os requisitos**)   
    
Sys.command "cls";;
print_string "Qual o ficheiro que pretende analisar?";;
print_newline();;
let file = scanf "%s" (fun x:string -> x);;

(**Após receber o nome do ficheiro, chama a função de validação**)
valida_ficheiro file;;

(**Apresenta relatório final ao utilizador**)
print_string "\nResumo de análise:\n";;
print_string "O ficheiro tem um total de ";;
print_int !count_linha;;
print_string " pacientes, ";;
print_int !risco_alto;;
print_string " de risco alto, ";;
print_int !risco_medio;;
print_string " de risco médio, ";;
print_int !risco_baixo;;
print_string " de risco baixo e ";;
print_int !semavaliacao;;
print_string " não poderam ser avaliados.\n\n";;