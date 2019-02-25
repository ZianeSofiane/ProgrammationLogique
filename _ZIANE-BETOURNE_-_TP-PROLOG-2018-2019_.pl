
% TP PROLOG


% Sofiane Ziane
% Aurelien Betourne

:-dynamic(addprofesseur/4).
:-dynamic(eleve/4).
:-dynamic(addmatiere/5).
:-dynamic(addsalle/4).
:-dynamic(addgroupe/4).
:-dynamic(seance/6).
:-dynamic(addclasse/7).

professeur(nom, prenom, discipline, identifiant).

eleve(nom, prenom, niveau, identifiant).

salle(nom, capacite, type , identifiant).

matiere(nom, discipline, type, niveau, identifiant).

groupe(nom, niveau,[eleve(nom, prenom, niveau, identifiant)| _] , identifiant).

seance(matiere(nom, discipline, type, niveau, identifiant), professeur(nom, prenom, discipline, identifiant), groupe(nom, niveau, liste-eleves, identifiant), salle(nom, capacite, type , identifiant), creneau, identifiant).

classe(nom, niveau, groupe(nom, niveau, [eleve(nom, prenom, niveau, identifiant)| _], identifiant), [professeur(nom, prenom, discipline, identifiant)| _], [matiere(nom, discipline, type, niveau, identifiant) | _], [seance(matiere(nom, discipline, type, niveau, identifiant), professeur(nom, prenom, discipline, identifiant), groupe(nom, niveau, liste-eleves, identifiant), salle(nom, capacite, type , identifiant), creneau, identifiant)|_], identifiant).


%predicat pour ajouter un professeur
addprofesseur(X,Y,Z,W):- 
not(professeur(_,_,_,W)),
assert(professeur(X,Y,Z,W)).

%predicat pour ajouter un eleve
addeleve(X,Y,Z,W):- 
not(eleve(_,_,_,W)),
assert(eleve(X,Y,Z,W)).


%predicat pour ajouter une salle
addsalle(X,Y,Z,W):- 
not(salle(_,_,_,W)),
assert(salle(X,Y,Z,W)).

%predicat pour ajouter une matiere
addmatiere(A,X,Y,Z,W):- 
not(matiere(_,_,_,_,W)),
assert(matiere(A,X,Y,Z,W)).

%predicat pour ajouter un groupe
addgroupe(X,Y,[A,B,C,D],W):- 
not(groupe(_,_,_,W)),
assert(groupe(X,Y,[A,B,C,D],W)).

%predicat pour ajouter une seance
addseance([A,B,C,D,E],[F,G,H,I],[K,L,M,N],[P,Q,R,S],Z,W):- 
not(seance(_,_,_,_,_,W)),
assert([A,B,C,D,E],[F,G,H,I],[K,L,M,N],[P,Q,R,S],Z,W).


%%%%%%%%%%%%%%%%%%%%%%%

%predicat pour supprimer un professeur
removeprofesseur(X,Y,Z,W):- retract(professeur(X,Y,Z,W)).

%predicat pour supprimer un eleve
removeleve(X,Y,Z,W):- retract(eleve(X,Y,Z,W)).

%predicat pour supprimer une salle
removesalle(X,Y,Z,W):- retract(salle(X,Y,Z,W)).

%predicat pour supprimer une matiere
removematiere(A,X,Y,Z,W):- retract(matiere(A,X,Y,Z,W)).

%predicat pour supprimer un groupe
removegroupe(X,Y,Z,W):- retract(groupe(X,Y,Z,W)).

%predicat pour supprimer une seance
removeseance(A,B,X,Y,Z,W):- retract(seance(A,B,X,Y,Z,W)).




%predicat qui ajoute un élève a un groupe

updategroupe(J,X,Y,[A,B,C,D],W):-  
removegroupe(J,_,_,_),
addgroupe(X,Y,[A,B,C,D],W).



%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%

listeseanceprof(A,X,Y,Z,W):-
seance(A,[X,Y,Z,W],_,_,_,_].


listeseancesalle(A,X,Y,Z,W):-
seance(A,_,_,[X,Y,Z,W],_,_].

listeseancegroupe(A,X,Y,Z,W):-
seance(A,_,[X,Y,Z,W],_,_,_].

%%%%%%%%%%%%%%%%

jour(X,_):-
X==1,
write('lundi').

jour(X,_):-
X==2,
write('mardi').

jour(X,_):-
X==3,
write('mercredi').

jour(X,_):-
X==4,
write('jeudi').

jour(X,_):-
X==5,
write('vendredi').

jour(X,_):-
X==5,
write('samedi').

%%%%%%%%%%%%%%%%

horaire(_,Y):-
Y==1,
write('8h-10h').

horaire(_,Y):-
Y==2,
write('10h-12h').

horaire(_,Y):-
Y==3,
write('14h-16h').

horaire(_,Y):-
Y==4,
write('16h-18h').

%%%%%%%%%%%%%%%

seanceconflit(seance(_, P1, G1, S1, _, _), seance(_,P2,G2,S2,_,_)):-
P1=\=P2,
G1=\=G2,
S1=\=S2.

%%%%%%%%%%%%%%%


edtprof(M,P,G,S,H,J):-
seance([M,_,_,_,_], [P,_,_,_],[G,_,_,_],[S,_,_,_],creneau(horaire(_,H),jour(J,_)),_),
writeln(P),
write(M,G,S,H,J).

edtsalle(S,J,H,M,P,G):-
seance([M,_,_,_,_], [P,_,_,_],[G,_,_,_],[S,_,_,_],creneau(horaire(_,H),jour(J,_)),_),
writeln(S).
write(M,P,G,H,J).

%%%%%%%%%%%%%%


addclasse(A,B,C,D,E,F,W):- 
not(classe(_,_,_,_,_,_,W)),
assert(classe(A,B,C,D,E,F,W)).




% Partie 2 Manipulation Arbre

:- dynamic(noeud/2).

%verifie si un element appartient a la liste

appartient(X, [X | _]).
appartient(X, [_ | L]) :- appartient(X, L).


% Creation de l'arbre

noeud(a, [b, c]).
noeud(b, [d, e, f]).
noeud(c, []).
noeud(d, []).
noeud(e, [g, h]).
noeud(f, []).
noeud(g, []).
noeud(h, []).

% verifie si les noeuds donnes existent
existent([]).
existent([X | L]) :-
  noeud(X, _),
  existent(L).


% Ajoute un noeud de valeur V et de fils L a l'arbre

ajouter_noeud(V, L) :-
  not(noeud(V, _)), % On verifie que le noeud n'existe pas deja
  existent(L), % On verifie que les fils existent
  assert(noeud(V, L)). % Puis on ajoute le noeud

%Supprime le noeud de valeur V et ses fils.
supprimer_noeud(V) :-
  noeud(V, L), % On verifie que le noeud de valeur V existe
  retract(noeud(V, _)), % On supprime le noeud
  supprimer_noeuds(L). % Puis on supprime ses fils

% Supprime les noeuds contenus dans la liste L
supprimer_noeuds([]).
supprimer_noeuds([X | L]) :-
  supprimer_noeud(X), % On supprime le noeud en tete de liste
  supprimer_noeuds(L). % On appelle le predicat pour supprimer le reste de la liste

%Retourne le nombre de noeuds
compter_noeuds(NB) :-
  findall(_, noeud(_, _), L),
  length(L, NB).

%Effectue le parcours prefixe de l'arbre ‡ partir d'un noeud N
prefixe(N) :-
  noeud(N, L),
  write(N),    % On affiche le noeud N
  write("\n"),
  prefixe(L). % parcours prefixe des fils de N

% Effectue le parcours prefixe d'une liste de noeuds L
prefixe([]).
prefixe([X | L]) :-
  prefixe(X), % parcours prefixe du premier noeud
  prefixe(L). % parcours prefixe du reste de la liste

%Effectue le parcours postfixe de l'arbre ‡ partir d'un noeud N
postfixe(N) :-
  noeud(N, L),
  postfixe(L), %parcours postfixe des fils de N
  write(N),    % On affiche le noeud N
  write("\n").

% parcours postfixe d'une liste de noeuds L
postfixe([]).
postfixe([X | L]) :-
  postfixe(X), % parcours postfixe du premier noeud de la liste
  postfixe(L). % parcours postfixe du reste de la liste


% parcours en largeur ‡ partir de la liste de noeuds L.
parcours_largeur([]).
parcours_largeur([N | F]) :-
  noeud(N, L),
  write(N),    % On ecrit le noeud N
  write("\n"),
  append(F, L, F2), % On ajoute les fils du noeud a† la file
  parcours_largeur(F2). % Puis on effectue le parcours en largeur de cette nouvelle file

% parcours en largeur de l'arbre a partir d'un noeud N.
parcours_largeur(N) :-
  parcours_largeur([N]).


% Verifie si B est fils de A
fils(A, B) :-
  noeud(A, L),
  appartient(B, L).

% Verifie si B est descendant de A
descendant(A, B) :-
  noeud(A, L),
  (fils(A, B) ; descendant(B, L)).

% Verifie si A est un descendant d'un noeud de la liste L
descendant(A, [X | L]) :-
  descendant(X, A) ;
  descendant(A, L).


% Verifie si A et B ont pour ancetre commun X
ancetre_commun(X, A, B) :-
  descendant(X, A),
  descendant(X, B).


