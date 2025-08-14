open System
open System.Text.RegularExpressions

// Define the SQL keywords from your query
let sqlKeywords = [
    "SELECT"; "DISTINCT"; "COUNT"; "AS"; "MIN"; "MAX";
    "FROM"; "WHERE"; "IS"; "NOT"; "NULL"; "AND";
    "EXTRACT"; "HOUR"; "GROUP"; "BY"; "ORDER"; "DESC"; "DATE"
]

// Sort keywords by length (longest first) to avoid partial matches
let sortedKeywords =
    sqlKeywords
    |> List.sortByDescending (fun k -> k.Length)

// Function to check if a character is a valid identifier character
let isIdentifierChar (c: char) =
    Char.IsLetterOrDigit(c) || c = '_'

// Function to check if keyword replacement should happen at this position
let shouldReplaceKeyword (text: string) (position: int) (keywordLength: int) =
    let beforePos = position - 1
    let afterPos = position + keywordLength

    // Check character before keyword
    let beforeIsIdentifier =
        if beforePos >= 0 && beforePos < text.Length then
            isIdentifierChar text[beforePos]
        else false

    // Check character after keyword
    let afterIsIdentifier =
        if afterPos >= 0 && afterPos < text.Length then
            isIdentifierChar text[afterPos]
        else false

    // Only replace if:
    // 1. Keyword is at start/end of string, OR
    // 2. Keyword is preceded/followed by non-identifier characters (like spaces, operators, etc.)
    // 3. But NOT if it's completely surrounded by identifier characters (like inside "FinishedAtLocal")

    match beforeIsIdentifier, afterIsIdentifier with
    | true, true -> false    // Keyword is inside an identifier - don't replace
    | true, false -> true    // Keyword at end of identifier - replace and add space after
    | false, true -> true    // Keyword at start of identifier - replace and add space before
    | false, false -> true   // Keyword is standalone - replace

// Smart keyword replacement function
let addSpacesAroundKeywordsSmart (queryString: string) =
    let upperQuery = queryString.ToUpper()
    let result = System.Text.StringBuilder()
    let mutable i = 0

    while i < upperQuery.Length do
        let mutable matchFound = false

        // Try to match each keyword at current position
        for keyword in sortedKeywords do
            let keywordUpper = keyword.ToUpper()
            if not matchFound && i + keywordUpper.Length <= upperQuery.Length then
                let substring = upperQuery.Substring(i, keywordUpper.Length)
                if substring = keywordUpper && shouldReplaceKeyword upperQuery i keywordUpper.Length then
                    // Check if we need space before
                    if result.Length > 0 && not (Char.IsWhiteSpace(result[result.Length - 1])) then
                        // Only add space before if the previous character suggests we need separation
                        let prevChar = result[result.Length - 1]
                        if isIdentifierChar prevChar || prevChar = ')' || prevChar = ']' then
                            result.Append(" ") |> ignore

                    result.Append(keywordUpper) |> ignore
                    i <- i + keywordUpper.Length

                    // Check if we need space after
                    if i < upperQuery.Length && not (Char.IsWhiteSpace(upperQuery[i])) then
                        let nextChar = upperQuery[i]
                        if isIdentifierChar nextChar || nextChar = '(' || nextChar = '[' then
                            result.Append(" ") |> ignore

                    matchFound <- true

        if not matchFound then
            result.Append(queryString[i]) |> ignore
            i <- i + 1

    // Clean up multiple consecutive spaces
    let cleanupRegex = Regex(@"\s+")
    cleanupRegex.Replace(result.ToString().Trim(), " ")

// Alternative approach using context-aware regex
let addSpacesAroundKeywordsContextAware (queryString: string) =
    let mutable result = queryString

    for keyword in sortedKeywords do
        let keywordUpper = keyword.ToUpper()
        let keywordLower = keyword.ToLower()

        // Pattern 1: Keyword at start of concatenated text (not preceded by identifier char)
        let pattern1 = sprintf @"(?<![A-Za-z0-9_])(?i)%s(?=[A-Za-z0-9_])" (Regex.Escape(keyword))
        let regex1 = Regex(pattern1)
        result <- regex1.Replace(result, sprintf "%s " keywordUpper)

        // Pattern 2: Keyword at end of concatenated text (not followed by identifier char)
        let pattern2 = sprintf @"(?<=[A-Za-z0-9_])(?i)%s(?![A-Za-z0-9_])" (Regex.Escape(keyword))
        let regex2 = Regex(pattern2)
        result <- regex2.Replace(result, sprintf " %s" keywordUpper)

        // Pattern 3: Standalone keywords (standard word boundaries)
        let pattern3 = sprintf @"\b(?i)%s\b" (Regex.Escape(keyword))
        let regex3 = Regex(pattern3)
        result <- regex3.Replace(result, keywordUpper)

    // Clean up multiple consecutive spaces
    let cleanupRegex = Regex(@"\s+")
    cleanupRegex.Replace(result.Trim(), " ")

// Test with problematic examples
let testCases = [
    "WHEREFinishedAtLocalIS NOT NULL"
    "SELECT FinishedAtLocal FROM table"
    "FinishedAtLocal>=17"
    "ANDEXTRACT(HOUR FROM FinishedAtLocal)"
    "ORDER BYovertime_shiftsDESC"
]

printfn "Testing smart approach:"
for testCase in testCases do
    let result = addSpacesAroundKeywordsSmart testCase
    printfn "Original: %s" testCase
    printfn "Result:   %s\n" result

// Test with your original problematic query
let originalQuery = "SELECT DISTINCT DriverName, DriverId, \nCOUNT(*) AS overtime_shifts,\nMIN(FinishedAtLocal) AS earliest_overtime_finish,\nMAX(FinishedAtLocal)AS latest_overtime_finish\nFROM '/Users/amirpanahi/Documents/work/lighthouse/Parquet_Files/Transport.ReportingDriverShift.parquet'\nWHEREFinishedAtLocalIS NOT NULL\n  ANDEXTRACT(HOUR FROM FinishedAtLocal) >= 17\nANDFinishedAtLocal >='2024-08-13'::DATE\nGROUP BY DriverName, DriverId\nORDER BYovertime_shiftsDESC, DriverName;"

printfn "Your original query with smart processing:"
let smartResult = addSpacesAroundKeywordsSmart originalQuery
printfn "%s" smartResult

printfn "\nWith context-aware regex:"
let regexResult = addSpacesAroundKeywordsContextAware originalQuery
printfn "%s" regexResult