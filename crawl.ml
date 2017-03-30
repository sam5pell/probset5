(*
                         CS 51 Problem Set 5
                   A Web Crawler and Search Engine
                             Spring 2017

The crawler, which builds a dictionary from words to sets of
links.
 *)

(* Rename modules for convenience *)
module WT = Webtypes ;;
module CS = Crawler_services ;;

(* Only look at pagerank if you plan on implementing it! *)
module PR = Pagerank ;;

(*----------------------------------------------------------------------
  Section 1: CRAWLER
 *)

(* TODO: Replace the implementation of the crawl function (currently
   just a stub returning the empty dictionary) with a proper index of
   crawled pages. Build an index as follows:

   Remove a link from the frontier (the set of links that have yet to
   be visited), visit this link, add its outgoing links to the
   frontier, and update the index so that all words on this page are
   mapped to linksets containing this url.

   Keep crawling until we've reached the maximum number of links (n) or
   the frontier is empty.
 *)

let rec add_words (l : WT.link) 
                  (dict : WT.LinkIndex.dict) 
                  (words: string list) 
                  : WT.LinkIndex.dict =
  match words with
  | [] -> dict
  | first :: rest -> 
    let new_dictionary = 
      match WT.LinkIndex.lookup dict first with
      | None -> WT.LinkIndex.insert dict first (WT.LinkSet.singleton l)
      | Some v -> WT.LinkIndex.insert dict first (WT.LinkSet.insert v l)
    in
    add_words l new_dictionary rest
;;

let rec crawl (n : int)
              (frontier : WT.LinkSet.set)
              (visited : WT.LinkSet.set)
              (d : WT.LinkIndex.dict)
              : WT.LinkIndex.dict =
  if n < 0 then d else 
  match WT.LinkSet.choose frontier with
    | None -> d
    | Some (first_link, new_frontier) -> 
        if WT.LinkSet.member visited first_link 
          then crawl n (WT.LinkSet.remove frontier first_link) visited d
        else 
            match CS.get_page first_link with
            | None -> crawl n new_frontier visited d
            | Some page -> crawl (n-1) (WT.LinkSet.union new_frontier page.links)
                           (WT.LinkSet.insert visited first_link) 
                           (add_words page.url d page.words)
  ;;

let crawler (num_pages_to_search : int) (initial_link : WT.link) =
  crawl num_pages_to_search
    (WT.LinkSet.singleton initial_link)
    WT.LinkSet.empty
    WT.LinkIndex.empty ;;
