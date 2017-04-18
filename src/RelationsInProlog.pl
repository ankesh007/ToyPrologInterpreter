male("Shantanu").
male("Bhishm").
male("Prativindhya").
male("Chitrangad").
male("Vichitraveerya").
male("Dhrutarastra").
male("Duryodhan").
male("Dushala").
male("Dussan").
male("Satanika").
male("Srutakirti").
male("Sutasoma").
male("Pandu").
male("Yudhisthir").
male("Bhim").
male("Arjun").
male("Nakul").
male("Sahdev").
male("Srutakarma").
male("Vidur").
male("Abhimanyu").
male("Karn").
female("Madri").
female("Maid").
female("Kunti").
female("Subhadra").
female("Ambika").
female("Ganga").
female("Gandhari").
female("Satyavati").
female("Ambalika").
female("Draupadi").


married("Shantanu","Ganga").
married("Shantanu","Satyavati").
married("Vichitraveerya","Ambika").
married("Vichitraveerya","Ambalika").
married("Vichitraveerya","Maid").
married("Dhrutarastra","Gandhari").
married("Pandu","Kunti").
married("Pandu","Madri").
married("Arjun","Subhadra").
married("Bhim","Draupadi").
married("Nakul","Draupadi").
married("Sahdev","Draupadi").
married("Bhim","Draupadi").
married("Arjun","Draupadi").


married("Ganga","Shantanu").
married("Satyavati","Shantanu").
married("Ambika","Vichitraveerya").
married("Ambalika","Vichitraveerya").
married("Maid","Vichitraveerya").
married("Gandhari","Dhrutarastra").
married("Kunti","Pandu").
married("Madri","Pandu").
married("Subhadra","Arjun").
married("Draupadi","Bhim").
married("Draupadi","Nakul").
married("Draupadi","Sahdev").
married("Draupadi","Bhim").
married("Draupadi","Arjun").

child("Bhishm","Shantanu").
child("Chitrangad","Shantanu").
child("Vichitraveerya","Shantanu").
child("Dhrutarastra","Vichitraveerya").
child("Pandu","Vichitraveerya").
child("Vidur","Vichitraveerya").
child("Duryodhan","Dhrutarastra").
child("Dushala","Dhrutarastra").
child("Dussan","Dhrutarastra").
child("Yudhisthir","Pandu").
child("Bhim","Pandu").
child("Arjun","Pandu").
child("Nakul","Pandu").
child("Sahdev","Pandu").
child("Abhimanyu","Arjun").
child("Prativindhya","Arjun").
child("Sutasoma","Yudhisthir").
child("Srutakirti","Bhim").
child("Satanika","Nakul").
child("Srutakarma","Sahdev").


child("Bhishm","Ganga").
child("Chitrangad","Satyavati").
child("Vichitraveerya","Satyavati").
child("Dhrutarastra","Ambika").
child("Pandu","Ambalika").
child("Vidur","Maid").
child("Duryodhan","Gandhari").
child("Dushala","Gandhari").
child("Dussan","Gandhari").
child("Yudhisthir","Kunti").
child("Bhim","Kunti").
child("Arjun","Kunti").
child("Nakul","Madri").
child("Sahdev","Madri").
child("Abhimanyu","Subhadra").
child("Prativindhya","Draupadi").
child("Sutasoma","Draupadi").
child("Srutakirti","Draupadi").
child("Satanika","Draupadi").
child("Srutakarma","Draupadi").

son(X,Y):-male(X),child(X,Y).
daughter(X,Y):-female(X),child(X,Y).
father(X,Y):-male(X),child(Y,X).
mother(X,Y):-female(X),child(Y,X).
grandfather(X,Y):-father(X,Z),father(Z,Y).
grandmother(X,Y):-mother(X,Z),mother(Z,Y).
ancestor(X,Y):-child(Y,X).
ancestor(X,Y):-child(Y,Z),ancestor(X,Z).
descendant(X,Y):-ancestor(Y,X).
sibling(X,Y):- mother(Z,X),mother(Z,Y),father(G,X),father(G,Y),not(X=Y).
half_sibling(X,Y):- mother(Z,X),mother(Z,Y),father(W,X),father(V,Y),not(W=V).
half_sibling(X,Y):- mother(W,X),mother(V,Y),father(Z,X),father(Z,Y),not(W=V).
cousin(X,Y):- child(X,Z),child(Y,W),sibling(Z,W).
cousin(X,Y):- child(X,Z),child(Y,W),half_sibling(Z,W).
cousin(X,Y):- child(X,Z),child(Y,W),cousin(Z,W).
uncle(X,Y):- child(Y,Z),(sibling(X,Z);half_sibling(X,Z);cousin(X,Z)),male(X).
uncle(X,Y):- child(Y,Z),sibling(W,Z),female(W),married(X,W).
aunt(X,Y):- married(Z,X),female(X),uncle(Z,Y).
nephew(X,Y):- uncle(Y,X) , male(X).
nephew(X,Y):- aunt(Y,X) , male(X).
neice(X,Y):- uncle(Y,X) , female(X).
neice(X,Y):- aunt(Y,X) , female(X).

relation(X,Y,"son"):-son(X,Y),!.
relation(X,Y,"daughter"):-daughter(X,Y),!.
relation(X,Y,"father"):-father(X,Y),!.
relation(X,Y,"mother"):-mother(X,Y),!.
relation(X,Y,"grandfather"):-grandfather(X,Y),!.
relation(X,Y,"grandmother"):-grandmother(X,Y),!.
relation(X,Y,"ancestor"):-ancestor(X,Y),!.
relation(X,Y,"descendant"):-descendant(X,Y),!.
relation(X,Y,"sibling"):-sibling(X,Y),!.
relation(X,Y,"half_sibling"):-half_sibling(X,Y),!.
relation(X,Y,"cousin"):-cousin(X,Y),!.
relation(X,Y,"uncle"):-uncle(X,Y),!.
relation(X,Y,"aunt"):-aunt(X,Y),!.
relation(X,Y,"nephew"):-nephew(X,Y),!.
relation(X,Y,"neice"):-neice(X,Y),!.
relation(X,Y,"married"):-married(X,Y),!.
relation(X,Y,"Not Related By means of defined Relations").

