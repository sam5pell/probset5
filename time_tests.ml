(* 
module AT = Askshiebs_tests ;;
module CS = Crawler_services ;;
module WT = Webtypes ;;
module CR = Crawl ;;
open CS51 ;;



let begin (module : string) : float =
	let start = Unix.time () in
	AT.time_crawler CR.crawler 42 CS.initial_link
    let end = Unix.time () in
    end -. start 
;;

let rec multiple_runs (f : unit -> float) (n : int) (l : float list) = 
	if n > 0 then
	  multiple_runs f (n - 1) ((f ()) :: l)
    else l
;;

let find_average (l : float list) = 
	let size = List.length l in
	let total = reduce (+.) 0.0 l in
	(total /. (float_of_int size))
;;


let test_link : WT.link = {host = ""; port = 80; path = "./simple-html/index.html"} ;;

let test () = 
	let _ = Printf.printf "Index has been constructed.\n" ;;

let time_index () =
	AT.time_crawler CR.crawler CS.num_pages_to_search CS.initial_link
;;

let try () = 
	CS51.call_reporting_time CR.crawler 42 CS.initial_link fib42
;;

*)

module AT = Askshiebs_tests ;;
module CS = Crawler_services ;;
module CR = Crawl ;;
module PR = Pagerank ;;
module WT = Webtypes ;;
module HS = Http_services ;;

(* server index ranks -- Runs a web server serving up a search engine
   with the provided index and ranks: opens a socket on the server
   port (specified on the command line), prepares it for listening,
   and then loops, accepting requests and sending responses. *)
let server (index : WT.LinkIndex.dict) (ranks : PR.RankDict.dict) =
  let _ = Printf.printf "Starting AskShiebs on port %d.\n" CS.server_port in
  let _ = Printf.printf "Press Ctrl-c to terminate AskShiebs.\n" in
  let _ = flush_all () in
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let sock_addr = Unix.ADDR_INET (Unix.inet_addr_any, CS.server_port) in
  let _ = Unix.setsockopt fd Unix.SO_REUSEADDR true in
  let _ = Unix.bind fd sock_addr in
  let _ = Unix.listen fd 5 in  (* at most 5 queued requests *)
  let rec server_loop () =
    (* allow a client to connect *)
    let (client_fd, _) = Unix.accept fd in
    let buf = Bytes.create 4096 in
    let len = Unix.recv client_fd buf 0 (String.length buf) [] in
    let request = String.sub buf 0 len in
    let _ = HS.process_request client_fd CS.root_dir request index ranks in
      Unix.close client_fd ;
      server_loop() in
    server_loop() ;;

(* main -- Crawl a bit of the web, building an index and ranks for the
   pages, and then serve up a sarch engine for the crawled
   material. *)
let main () =
  (* Want different random numbers every time. *)
  let _ = Random.self_init () in
  let _ = flush_all () in
  let _ = Printf.printf "Indexing %d pages.\n" CS.num_pages_to_search in
  (* test the crawler -- prints whether tests succeeded *)
  let _ = AT.crawler_tests () in
  (* Construct the index to pass to the server *)
  let index = 
    AT.time_crawler CR.crawler CS.num_pages_to_search CS.initial_link in 
  let _ = Printf.printf "Index has been constructed.\n" in
  let _ = Printf.printf "Computing pageranks.\n" in
  let ranks = CS.compute_pagerank index in
  (* let _ = AT.debug_pageranks ranks in *)
  let _ = Printf.printf "Pageranks computed.\n" in
  server index ranks ;;
