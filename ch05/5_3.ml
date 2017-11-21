(* 目的: 受け取った月 month と 日 day から
   その誕生日の星座を表す文字列を計算する *)
(* seiza: int -> int -> string *)
let seiza month day =
  if month == 1 then
    if day < 21 then "Capricorn" else "Aquarius"
  else if month == 2 then
    if day < 19 then "Aquarius" else "Pisces"
  else if month == 3 then
    if day < 21 then "Pisces" else "Aries"
  else if month == 4 then
    if day < 20 then "Aries" else "Taurus"
  else if month == 5 then
    if day < 21 then "Taurus" else "Gemini"
  else if month == 6 then
    if day < 22 then "Gemini" else "Cancer"
  else if month == 7 then
    if day < 23 then "Cancer" else "Leo"
  else if month == 8 then
    if day < 23 then "Leo" else "Virgo"
  else if month == 9 then
    if day < 23 then "Virgo" else "Libra"
  else if month == 10 then
    if day < 24 then "Libra" else "Scorpio"
  else if month == 11 then
    if day < 23 then "Scorpio" else "Sagittarius"
  else if month == 12 then
    if day < 22 then "Sagittarius" else "Capricorn"
  else ""


(* テスト *)
let test1 = seiza 1 25 = "Aquarius" ;;
let test2 = seiza 5 21 = "Gemini" ;;
let test3 = seiza 8 10 = "Leo" ;;
let test4 = seiza 12 21 = "Sagittarius" ;;
let test5 = seiza 12 22 = "Capricorn" ;;
