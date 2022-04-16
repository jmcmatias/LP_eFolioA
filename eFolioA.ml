
(* para compilar -> ocamlc str.cma -o  eFolioA eFolioA.ml*) 

let recipes_file = "receitasV1_encoded.csv"

let menu = ref 1


(*Funções Auxiliares*)

(*Remove o primeiro elemento de uma lista*)
let remove_list_first_element = function
  | [] -> [] 
  | h :: t -> t

let clear = function () -> Sys.command("clear")



(*
Datatype das receitas
*)
type recipe =
{
  number: string;
  name: string;
  prep_mode: string;
  ingredients: string list;
}

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

(*Função que devolve os ingredientes (faz split da lista a partir do elemento 3)
INPUT: Lista Receita
OUTPUT: Lista de Ingredientes e Quantidades*)
let get_ingredients_from_recipe list =
  let rec aux i acc = function
    | [] -> List.rev []
    | h :: t as l -> if i = 0 then  l
        else aux (i - 1) (h :: acc) t 
  in
  aux 3 [] list

(*Função para dividir a informação de uma string a cada ";"*)        
let rec split_string_by_semicolon l = match l with
| [] -> []
| s :: rest -> (Str.split (Str.regexp ";") s) :: split_string_by_semicolon rest

(*Função que transforma uma receita(lista) num tipo recipe *)
let get_recipe l:recipe =
  let rec_ingredients = get_ingredients_from_recipe l in
  {
    number = List.nth l 0;
    name = List.nth l 1;
    prep_mode = List.nth l 2;
    ingredients = rec_ingredients;
  } 

(*****************FUNÇÓES DE IMPRESSÃO***********)




(*Funções que imprimem a lista de recitas existentes*)
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
  print_string "| 2-Imprime uma receitas                                                |\n";
  print_string "| 3-Imprime a analise a 3 receitas, mostrando as quantidades totais de  |\n";
  print_string "|   cada ingrediente do conjunto de receitas                            |\n";
  print_string "|                                                                       |\n";
  print_string "| 0-Sair do programa                                                    |\n";
  print_string "-------------------------------------------------------------------------\n"


(*Função que imprime uma receita, com exceção dos ingredientes*)
let print_recipe recipe = print_string (""^ recipe.number ^" - "^ recipe.name ^"\n"^ recipe.prep_mode ^" ")

(*Funções para impressão de um ingrediente com a sua quantidade*)
let print_ingredient_line x y = print_string (""^x^": "^y^"\n ")   (* caso geral *)
let print_ingredient_line_odd x  = print_string (""^x^"\n")       (* Caso em que a lista de ingredientes tem número impar de elementos *)

(*Função que imprime a lista de ingredientes
INPUT: Lista de ingredientes
OUTPUT: impressão no ecran dos ingredientes e suas quantidades*)
let rec print_ingredient_list = function 
  | [] -> () 
  | x::(y::t) -> if x="" then print_ingredient_list t 
      else if y="" then begin
        print_ingredient_line_odd x;
        print_ingredient_list t;
      end else
        begin
          print_ingredient_line x y;
          print_ingredient_list t;
        end
  | h::t -> print_string h

(*Função que imprime a lista de receitas
INPUT: Lista de receitas (já separada por ';')
OUTPUT: impressão no ecran das receitas*)
let print_all_recipes recipes_list =
  print_string ("############### LISTA DE RECEITAS ###############\n");
  List.iter (
    fun recipe_l -> (
      match recipe_l with
      | [] -> print_string ("Nao existem receitas\n")
      | _ ->
        let recipe = get_recipe recipe_l in
        print_recipe recipe;
        print_string ("\n------------ Lista de Ingredientes ------------\n\n ");
        print_ingredient_list recipe.ingredients;
        print_string ("\n-----------------------------------------------\n\n");
        
    )
  ) recipes_list;
  print_newline();
;;

(*print_all_recipes list_recipes*)

let main = 
  let raw_recipes = remove_list_first_element (read_lines_file_to_list recipes_file) in  (* Coloca as linhas do ficheiro numa lista com a exceção da primeira *)
  let list_recipes = split_string_by_semicolon raw_recipes in                            (* Separa as strings por ";" para uma lista de listas*)
  while !menu=1 do
    clear;
    print_menu list_recipes;
    let opt = read_line () in
    match opt with
      "1" -> print_all_recipes list_recipes
    | "2" -> print_string "\nOpção 2"
    | "3" -> print_string "\nOpção 3"
    | "0" -> print_string "\nA Sair"; menu := 0
    | _ -> print_string "\n\nPor favor insira uma Opção Válida\n\n"
  done

