# cse505project
Open XSB
import this file:
  [parser].
Now you can run the system by 
  run.
Now you will get something like this:
--> 
Now you can write a statement 
--> 1984 is a novel
and you can ask questions:
--> What is a novel
answer([1984]).

As the dictionary used in this case is very small. There is a limit to things what a statement can have.
Noun: tom, jack, john, bill, terry, 1984.
They can be author, programmer, novel, program name.
They can meet each other or someone can write a novel or a program. Say it can halt.
Then you can ask who met whom or who wrote a novel or who is a author or a programmer.

Following is a set of line for demo. After that you can see in dictionay in code and accordingly input a statement.
-->1984 is a novel
-->bill is a author
-->bill wrote all the novel
-->who wrote 1984   ###### This will give answer as bill, as above you mentioned that bill wrote all the novels
-->tom is a programmer
-->tom meets every author
-->who did tom meet  ####### This will have bill as answer as he is a author.

