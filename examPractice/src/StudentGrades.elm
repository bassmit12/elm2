module StudentGrades exposing (..)

report : List (String, Int) -> String -> Maybe (String, List Int, Float)
report grades studentName =
    let
        -- Extract grades for the specified student
        studentGrades =
            List.filter (\(name, _) -> name == studentName) grades
                |> List.map Tuple.second

        -- Calculate the average grade
        averageGrade : List Int -> Float
        averageGrade gradesList =
            if List.isEmpty gradesList then
                0
            else
                List.sum (List.map toFloat gradesList) / toFloat (List.length gradesList)
    in
    if List.isEmpty studentGrades then
        Nothing
    else
        Just (studentName, studentGrades, averageGrade studentGrades)

