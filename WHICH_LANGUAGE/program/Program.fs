#light

// Name: Nemil Shah
// netid: nshah213
// UIN: 670897116
// U. of Illinois, Chicago
// CS 341, Fall 2018
// Project #05: Language prediction based on letter frequencies
//
// This program analyzes text from various languages, and computes
// letter frequencies.  These letter frequencies serve as a "barcode"
// that potentially identify the language when written. 


// explode:
//
// Given a string s, explodes the string into a list of characters.
// Example: explode "apple" => ['a';'p';'p';'l';'e']
//
let explode s =
  [for c in s -> c]


//
// implode
//
// The opposite of explode --- given a list of characters, returns
// the list as a string.
//
let implode L =
  let sb = System.Text.StringBuilder()
  List.iter (fun c -> ignore (sb.Append (c:char))) L
  sb.ToString()


//
// FileInput:
//
// Inputs text from a file line by line, returning this as a list
// of strings.  Each line is converted to lower-case.
//
let FileInput filename = 
  [ for line in System.IO.File.ReadLines(filename) -> line.ToLower() ]


//
// UserInput:
//
// This function reads from the keyboard, line by line, until 
// # appears on a line by itself.  The lines are returned as
// a list of strings; each line is converted to lower-case.
//
// NOTE: if the first line of input is blank (i.e. the user 
// presses ENTER), then input is read from the file 'input.txt'.
// Helpful when testing.
//
let rec _UserInput input =
  let line = System.Console.ReadLine()
  match line with
  | "#" -> List.rev input
  |  _  -> _UserInput (line.ToLower() :: input)

let UserInput() =
  let firstLine = System.Console.ReadLine()
  match firstLine with
  | "" -> FileInput @"./input.txt"
  | _  -> _UserInput [firstLine.ToLower()]
 
 
// Below there are 10 functions written by me.
  
// Function to Print the frequency count ------------1                   
let  rec  printFreqCounts L  =  
    match  L  with  
    |  []                                      ->  ()    //  read  this  as  "do  nothing,  return  void"  
    |  (letter,count)::rest  ->  printf "%A  "  count 
                                 printFreqCounts rest

// Function to Print the letters in order from high to low.--------------------2
let  rec   printFreqOrder L =
    match  L  with  
    |  []                                      ->  ()    //  read  this  as  "do  nothing,  return  void"  
    |  (letter,count)::rest  ->  printf "%c"  letter 
                                 printFreqOrder rest 
  

// Count function updates the list of frequency count for One particular string.-----------------3
// Returns the updated version of list of frequency count.
let rec count charList tuple =
   match charList with
   | [] ->  tuple        // After comparing and processing each character, tuple is the updated version of list of frequency count.
   | e::rest -> let newtuple = List.map ( fun (letter, count) -> if letter = e then    // If the character in charList matches with character of list of frequency count, 
                                                                 (letter, count+1)      // then increment the count.
                                                                 else
                                                                (letter, count) ) tuple 
                count rest newtuple
                
// Function updates the list of frequency count.-----------------------4
// After processing  all the list of strings for particular language, it finally returns the list of frequency count
// L2 is the list of frequency count.
let rec process_frequencyCount L1 L2 =
  match L1 with
  | [] ->  L2     // Return the frequency count list after processing all the strings of that lanuage.  
  |  e:: rest  -> let char = explode e   // Explode the string.
                  let updateL2 = count char L2   //  Returns the frequency count for one string in that list.
                  process_frequencyCount rest updateL2


// Function does the two job.--------------------------5
// Job 1 ---Creates the list of languages. Each elem is tuple (langauge name , list of frequency count)
// Job-2 ---For each language, creates the list of frequency count.
//  Recursive function is used to make list of languagelist from each file.
let rec makeLanguageList files lettersCount languageList =
      match  files  with 
       | [] ->  List.rev languageList
       | e:: rest -> let stringFile = FileInput e // Returns the file with list of strings.
                     let language = List.head stringFile
                     printfn ""
                     printf "%A: " language
                     let stringFile2 = List.tail stringFile   // To skip the first line.
                     let lettersCount1 = process_frequencyCount stringFile2 lettersCount  // process function will return the list of tuple of frequency count/
                     printFreqCounts lettersCount1     // Prints the count for each letter for that language.
                     makeLanguageList rest lettersCount ((language, lettersCount1) :: languageList)
 
// Function prints the letters of each language by frequency order from high to low.-------------------------6
// Recursive function is used to print it for all languages.
let rec frequency_order languageList =
         match   languageList  with 
         | [] ->  []
         | (language, countList) :: rest ->  printfn ""
                                             printf "%A: " language
                                             let countList2 = countList
                                             let sort =  List.sortByDescending ( fun (letter, count) -> count) countList2  // Sort the list by desscindg.
                                             printFreqOrder sort   // Print the letter by order defined above.
                                             frequency_order rest


// Recursive function returing the number.-----------------------------9
// Number returned indicates the postion for that character in list.
// For eg List is---> ebadvdkvjlkdjvlk. For character 'a' input, it returns the number 3 indicating its positions.
let rec getPosition x L p =
         match L  with 
         | [] ->  0
         | (letter, count):: rest  when letter = x  -> p
         | (letter, count):: rest -> getPosition x rest p+1

// Function to sort the list by descending order.------------------------------10
let rec change_list L =
   let sort =  List.sortByDescending ( fun (letter, count) -> count)  L
   sort


// Function to calculate the absolute  differences of frequency count between userList and datalist (each from training files) by index positions.--------8
// Function returns the list of differences for each letter. For eg [ 4;3;5;7.......]  4 is difference for a, 3 is for b, and so on.
// Recursive function to calcluate the difference for each letter. 
let rec diff letters dataList userList make =
        match letters  with 
        | [] ->  List.rev make               // Returns the list of difference value  for each letter.             
        | e:: rest -> let dataListNew = change_list dataList      // Reverse the datalist.
                      let userListNew = change_list userList       // Reverse the userlist.
                      let p = getPosition e dataListNew  1        // Get the postion of 'a' in datalist. For now e is 'a'.
                      let q = getPosition e userListNew  1          // Get the postion of 'a' in usrlist.
                      let sub =                                  // Calc. absolute diff.
                          if p >= q then
                            p-q
                          else
                            q-p
                      diff rest dataList userList (sub:: make)   
 
// Function recursively evaluates each language.------------------------------------7
// Function returns the List of tuple. Tuple is for each langauge, it has evaluation list of tuple.
// Evaluation list of tuple is defined as for each letter, it has corresponding value of sum of differences acc. to threshold set.
//  For example [ (a,8); (b,5).......(c,13)]
let rec  evaluate languageList letters lettersCount2 threshold  newSet  =
     match languageList with 
     | [] ->  List.rev newSet    // List of all languages tuple after evaluating each language.
     | (language, countList) :: rest -> let diff_list = diff letters countList lettersCount2 []   // Returns the list of evaluation by calc. differences.
                                        let total = List.filter ( fun (e) ->  e > threshold) diff_list   // Filter the elements by threshold.
                                        let total2 = List.sum total    // Suming the differences.
                                        evaluate rest letters lettersCount2  threshold ((language, total2):: newSet )

// *********************************************************************** //
//
// Main:
//
[<EntryPoint>]
let main argv =
  printfn "** Training... **"
  printfn ""
  
  let letters = [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z']
  //printfn "alpha are: %A" letters
  
  // letters count is list of tuples. For each letter, it has corresponding count for that letter.  Eg [ (a,6); (b,4); ....(z,4)]
  let lettersCount = List.map ( fun e -> ( e, 0)) letters    // List of frequency Count for each letter.
  //printfn "alpa  counts  are: %A" lettersCount
  //
  let files = [ for filename in System.IO.Directory.GetFiles(@"./training") -> filename]
//  printfn "Training files: %A" files

  let files2 = List.sort files   // Sort the file to get the correct sequence.

  printfn "** Letter Frequency Counts (A->Z) **"
  
  // Returns the list of all languages. ----------------------TASK-1
  // Each language is a tuple of  (langauge, list of frequency count.)
  let languageList = makeLanguageList files2 lettersCount []   
  printfn ""
  printfn ""
  
  printfn "** Letter Frequency Order (High->Low) **"

  // Prints the out of letter from highest to lowest frequence ------------TASK-2
  frequency_order languageList
  printfn ""
  printfn ""

  //
  // Here we get text from the user, analyze, and guess the language:--------------------------TASK-3
  //
  printfn "Please enter text, followed by # (default = 'input.txt')> "
  let text = UserInput()
  printf  "input: "
  
  // Function returns the update version of list of frequency count.
  let lettersCount2 = process_frequencyCount text lettersCount    // letterscount2 is updated version count of letters..   
  printFreqCounts lettersCount2   // Print the counts of each letter.
   
  printfn ""
  printf  "input: "
  let sort =  List.sortByDescending ( fun (letter, count) -> count)  lettersCount2   // Sort from high to low according to count.
  printFreqOrder sort                 // Print the letters from frequency high to low.
  printfn ""
  printfn ""
  
  
  //
  printf "Enter difference threshold (default = 4)> "
  let s = System.Console.ReadLine()
  let threshold = if s = "" then 4 else int(s)
  printfn ""

  // Evaluate function returns the list of tuples.
  // List of tuple is defined as "for each letter--it has correspoding value which is sum of differences between userlist and datalist. ----------TASK-4
  // For example [ (a,8); (b,5).......(c,13)]
  let result = evaluate languageList letters lettersCount2 threshold []
  let result2 = List.sortByDescending  ( fun (letter, count) -> count)  result
  let result3 = List.rev result2
  let finalLanguage = List.head result3          // Finallanguage is result to output.
 // print "diffs: "
  printfn "diffs: %A" result3 
  
  printfn ""

  printfn "** Input language: %A"  (fst finalLanguage) 
  printfn ""
  //
  0
