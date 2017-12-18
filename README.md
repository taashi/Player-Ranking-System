# Player-Ranking-System
Please clone both the files for the project to work at your end. The extras file is required for the project file to work.
This code involves ranking individuals or teams in a sport such as chess whose contests result in a win or tie (draw).
The system treats a tie as a win for both competitors and also as a loss for both competitors.
The ranking system specified below uses this definition:
Given a list of outcomes, a competitor A outranks a competitor B if any of the following are true:
One of the outcomes shows that A has defeated B.
One of the outcomes shows that A and B have tied.
There is a competitor C that outranks B according to the list of outcomes, and there is an outcome 
that shows A has defeated or tied C.
The power ranking has the following characterstics-
If competitor A is outranked by fewer competitors than competitor B, then the power-ranking of A is higher than the power-ranking of B.
If competitor A is outranked by the same number of competitors as competitor B, but competitor A outranks more competitors than competitor B, then the power-ranking of A is higher than the power-ranking of B.
If competitor A is outranked by the same number of competitors as competitor B, and competitor A also outranks the same number of competitors as competitor B, and competitor A has a higher non-losing percentage than competitor B, then the power-ranking of A is higher than the power-ranking of B.
If competitor A is outranked by the same number of competitors as competitor B, and competitor A also outranks the same number of competitors as competitor B, and competitor A has the same non-losing percentage as competitor B, and Racket's string<? function considers the name of competitor A to be less than the name of competitor B, then the power-ranking of A is higher than the power-ranking of B.
