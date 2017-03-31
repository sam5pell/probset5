(*print_string "hi there stranger\n";;



module AT = Askshiebs_tests ;;
module CS = Crawler_services ;;
module CR = Crawl ;;
module PR = Pagerank ;;
module WT = Webtypes ;;
module HS = Http_services ;;

open Askshiebs_tests ;;    
open Crawler_services ;;
open Pagerank ;;
open Crawl ;;


let main () = 
  let num_pages_to_search = 42 in
  let _ = Random.self_init () in
  let _ = Printf.printf "Indexing %d pages.\n" num_pages_to_search in
  time_crawler crawler num_pages_to_search initial_link ;;

main ();;

*)