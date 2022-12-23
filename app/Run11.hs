module Run11 where

import Day11

input = ["Monkey 0:","Starting items: 91, 66","Operation: new = old * 13","Test: divisible by 19","If true: throw to monkey 6","If false: throw to monkey 2","","Monkey 1:","Starting items: 78, 97, 59","Operation: new = old + 7","Test: divisible by 5","If true: throw to monkey 0","If false: throw to monkey 3","","Monkey 2:","Starting items: 57, 59, 97, 84, 72, 83, 56, 76","Operation: new = old + 6","Test: divisible by 11","If true: throw to monkey 5","If false: throw to monkey 7","","Monkey 3:","Starting items: 81, 78, 70, 58, 84","Operation: new = old + 5","Test: divisible by 17","If true: throw to monkey 6","If false: throw to monkey 0","","Monkey 4:","Starting items: 60","Operation: new = old + 8","Test: divisible by 7","If true: throw to monkey 1","If false: throw to monkey 3","","Monkey 5:","Starting items: 57, 69, 63, 75, 62, 77, 72","Operation: new = old * 5","Test: divisible by 13","If true: throw to monkey 7","If false: throw to monkey 4","","Monkey 6:","Starting items: 73, 66, 86, 79, 98, 87","Operation: new = old * old","Test: divisible by 3","If true: throw to monkey 5","If false: throw to monkey 2","","Monkey 7:","Starting items: 95, 89, 63, 67","Operation: new = old + 2","Test: divisible by 2","If true: throw to monkey 1","If false: throw to monkey 4"]
  
main :: IO ()
main = do
  print $ day11a input
  print $ day11b input

