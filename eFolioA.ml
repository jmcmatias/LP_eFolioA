
(* para compilar -> ocamlc str.cma -o  eFolioA eFolioA.ml*) 

let recipes_file = "receitas2.csv"

let menu = ref 1
let opt2 = ref ""
let s = ref 0

let n_rec = 3 (*número de receitas a ser analisadas para as quantidades totais de cada ingrediente*)

(*Datatype das receitas*)
type recipe =
{
  number: string;
  name: string;
  prep_mode: string;
  ingredients: string list list;
}

(*************   Funções Auxiliares   *************)

(*Remove o primeiro elemento de uma lista*)
let remove_list_first_element = function
  | [] -> [] 
  | h :: t -> t

(*Limpa o ecran*)
let clear = function x -> ignore(Sys.command("clear")+x)

(*Recebe e valida um inteiro
Caso retorne zero, significa que ou foi inserido zero ou então não foi inserido um inteiro*)
let input_int n = 
  try
    read_int()
  with Failure _-> -1

(*Função que converte um array numa lista*)
let array_to_list a =
  Array.fold_right List.cons a []


(*Função que imprime uma lista*)
(*Apenas foi utilizada para DEBUG*)
let rec print_list = function
  | [] -> ()
  | h::t -> print_string (" "^h^" "); print_list t

(*Função que conta o número de elementos numa lista*)
let count_dup l = 
  let sl = List.sort compare l in
  match sl with
  | [] -> []
  | hd::tl -> 
      let c,acc,x = List.fold_left (fun (c,acc,x) y -> if y = x then c+1,acc,x else 1,(x,c)::acc, y) (1,[],hd) tl in
      (x,c)::acc ;;
  
(*************   Funções de tratamento de Listas   *************)

(*Função de leitura do ficheiro
Input: name - Nome do ficheiro
Output: Lista de strings com o conteudo de cada linha*)
let read_lines_file_to_list name : string list =
  if Sys.file_exists (name) then
    begin
      let ic = open_in name in
      try
        let try_read () =
          try Some (input_line ic) with End_of_file -> None in
          let rec line acc = match try_read () with
          | Some s -> line ( s :: acc)
          | None -> close_in_noerr ic; List.rev acc in
          line []
        with e -> 
          close_in_noerr ic;
          []
        end
      else
        []

(*Função que devolve os ingredientes (faz split da lista a partir do elemento n)
INPUT: Lista Receita
OUTPUT: Lista de Ingredientes e Quantidades*)
let split_from_n n list =
  let rec aux i acc = function
    | [] -> List.rev []
    | h :: t as l -> if i = 0 then  l
        else aux (i - 1) (h :: acc) t 
  in
  aux n [] list

(*Função para dividir a informação de uma string a cada ";"*)        
let rec split_string_by_semicolon l = match l with
| [] -> []
| h :: t -> (Str.split (Str.regexp ";") h) :: split_string_by_semicolon t

(*Função que agrupa ingredientes com quantidades a partir de uma lista "flattened" de ingrediente;quantidade;ingrediente;quan...
INPUT: lista de strings
OUTPUT: lista de listas com pares*)
let rec group_ingredient_quantity = function
  | x :: (y :: t) -> if x="" then (group_ingredient_quantity t) else [x;y] :: (group_ingredient_quantity t)
  | [] | _::[] -> []

(*Função que transforma uma receita(lista) num tipo recipe *)
let get_recipe l:recipe =
  let rec_ingredients = group_ingredient_quantity (split_from_n 3 l) in
  {
    number = List.nth l 0;
    name = List.nth l 1;
    prep_mode = List.nth l 2;
    ingredients = rec_ingredients;
  } 

(*****************FUNÇÓES DE IMPRESSÃO***********)

(*imprime o info de uma receita, numero e nome*)
let print_recipe_info  = function
  | []->()
  | x :: y :: t -> print_string ("            "^x^"-"^y^"\n") 
  | x -> ()
           
(*imprime o info de todas as receitas*)
let rec print_recipe_list = function 
  []->()
  |x::t->print_recipe_info x; print_recipe_list t

(*Função que imprime o Menu*)
let print_menu rl= 
  print_string "############################# Menu Receitas #############################\n";
  print_string "                      As Receitas Disponiveis Sao:                       \n";
  print_string "-------------------------------------------------------------------------\n";
  print_recipe_list rl;
  print_string "-------------------------------------------------------------------------\n";
  print_string "| Selecione uma das opcoes digitanto o numero da opcao seguido de enter |\n";
  print_string "-------------------------------------------------------------------------\n";
  print_string "| 1-Imprime todas as receitas                                           |\n";                                       
  print_string "| 2-Imprime uma receita                                                 |\n";
  print_string "| 3-Imprime a analise a 3 receitas, mostrando as quantidades totais de  |\n";
  print_string "|   cada ingrediente do conjunto de receitas                            |\n";
  print_string "|                                                                       |\n";
  print_string "| 0-Sair do programa                                                    |\n";
  print_string "-------------------------------------------------------------------------\n"


(*Função que imprime uma receita, com exceção dos ingredientes*)
let print_recipe_with_prep_mode recipe = print_string (""^ recipe.number ^" - "^ recipe.name ^"\n"^ recipe.prep_mode ^" ")

(*Funções para impressão de um ingrediente com a sua quantidade*)
let rec print_ingredient_info = function
  | [] -> ()
  | h::t -> if h="" then print_list t 
  else if t = [] then begin print_string (""^h^""); print_ingredient_info t end
  else begin print_string (" "^h^": "); print_ingredient_info t end
  
(*Função que imprime a lista de ingredientes
INPUT: Lista de ingredientes
OUTPUT: impressão no ecran dos ingredientes e suas quantidades*)
let rec print_ingredient_list = function 
  | [] -> () 
  | h::t -> print_ingredient_info h; print_string "\n"; print_ingredient_list t 

(*Função que imprime uma receita completa*)
let print_full_recipe recipe = 
  print_recipe_with_prep_mode recipe;
  print_string ("\n------------ Lista de Ingredientes ------------\n\n");
  print_ingredient_list recipe.ingredients;
  print_string ("\n-----------------------------------------------\n\n")

(*Função que imprime a lista de receitas
INPUT: Lista de receitas (já separada por ';')
OUTPUT: impressão no ecran das receitas*)
let print_all_recipes recipes_list =
  print_string ("\n\n############### LISTA DE RECEITAS ###############\n\n");
  List.iter (
    fun recipe_l -> (
      match recipe_l with
      | [] -> print_string ("Nao existem receitas\n")
      | _ ->
        let recipe = get_recipe recipe_l in
        print_full_recipe recipe
    )
  ) recipes_list;
  print_newline();
;;


(*Função que imprime uma receita pré-selecionada
INPUT: Lista de receitas (já separada por ';') cada receita é também uma lista
OUTPUT: impressão no ecrã da receita seleccionada*)
let print_one_recipe recipes_list =
  let n = ref (input_int 0) in
  let quit_loop = ref false in
  while not !quit_loop
  do
    if (!n>0) then
      begin
        try
          let selected_recipe = get_recipe (List.nth recipes_list (!n-1)) in
          print_full_recipe selected_recipe;
          quit_loop:=true;
        with Failure nth -> print_string "\nInsira uma opção válida\n"; n := input_int 0
      end
    else 
      begin
        print_string "Insira uma opção válida\n";
        n := input_int 0
      end
  done
;;



(*Função que elabora o relatório geral dos ingredientes
INPUT: Array de listas com os ingredientes de cada receita em cada elemento*)

let rec print_ingredient_report = function
  | [] ->()
  | (h,c)::t -> print_int c; print_string " x "; print_ingredient_info h; print_string "\n"; print_ingredient_report t

(*Função que retorna o resultado da análise do total da quantidade de cada ingrediente em tres receitas designadas
INPUT: Lista de receitas (já separada por ';') cada receita é também uma lista
OUTPUT: Array em que cada elemento contem uma lista dos ingredientes de cada receita escolhida*)
let selected_recipes list_recipes =
  let recipes_to_analyze  = Array.make n_rec  [] in
  let n = ref 0 in
  print_string "\nInsira o número da 1ª Receita\n";
  n := input_int 0;
  let r = ref n_rec in
  let n_selection = ref 0 in
  let quit_loop = ref false in
  while not !quit_loop
  do
    if (!n>0) then
      begin
        try
          let recipe_chosen =  get_recipe(List.nth list_recipes (!n-1)) in
          recipes_to_analyze.(!n_selection)<-  recipe_chosen.ingredients;
          n_selection := !n_selection + 1;
          r := !r - 1;
          if !r=0 then 
            begin
              print_ingredient_report (count_dup (List.flatten (array_to_list recipes_to_analyze)));
              quit_loop:=true
            end
          else
            begin
              print_string "\nInsira o número da "; print_int (!n_selection+1); print_string"ª receita\n";
              n := input_int 0;
            end
        with Failure nth -> print_string "\nInsira uma opção válida (if)\n"; n := input_int 0
      end
    else 
      begin
        print_string "Insira uma opção válida(else)\n";
        n := input_int 0
      end
    done
  ;;


(*Menu Principal*)
let main = 
  let raw_recipes = remove_list_first_element (read_lines_file_to_list recipes_file) in  (* Coloca as linhas do ficheiro numa lista com a exceção da primeira *)
  let list_recipes = split_string_by_semicolon raw_recipes in                            (* Separa as strings por ";" para uma lista de listas*)
  clear 1;
  while !menu=1 do
    s := 0;
    print_menu list_recipes;
    let opt = read_line () in
    match opt with
      "1" -> clear 1; print_all_recipes list_recipes
    | "2" -> print_string "\n\n->>> Selecione o número da receita que pretende imprimir:\n\n"; print_one_recipe list_recipes
    | "3" -> print_string "\n\n->>> Insira as três receitas que pretende analisar:\n\n"; selected_recipes list_recipes
    | "0" -> clear 1; print_string "\n\nA Sair...\n\n"; menu := 0
    | _   -> clear 1; print_string "\n\nPor favor insira uma Opção Válida\n\n"
  done