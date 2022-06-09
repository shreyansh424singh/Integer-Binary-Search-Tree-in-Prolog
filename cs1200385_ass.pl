ibt(empty).
ibt(node(N,L,R)) :- integer(N), ibt(L), ibt(R).

size(empty, 0).
size(node(_,LBT,RBT), N) :- size(LBT,N1), 
                            size(RBT,N2),
                            N is N1+N2+1.

height(empty,0).
height(node(_,LBT,RBT),N) :- height(LBT,N1),
                             height(RBT,N2),
                             N is max(N1, N2)+1.

preorder(empty,[]).
preorder(node(N,LBT,RBT), [N|L]) :- preorder(LBT,LT),
                                    preorder(RBT,RT),
                                    append(LT,RT,L).

inorder(empty,[]).
inorder(node(N,LBT,RBT), L) :- inorder(LBT,LT),
                               inorder(RBT,RT),
                               append(LT,[N|RT],L).

postorder(empty,[]).
postorder(node(N,LBT,RBT),L) :- postorder(LBT,LT),
                                postorder(RBT,RT),
                                append(LT,RT,L1),
                                append(L1,[N],L).

trPreorder(empty,[]).
trPreorder(BT, L) :- append([BT],[],L1),
                     tailpr(L1,L).

tailpr([],[]).                                %implements tail recursion
tailpr([node(N,empty,empty)],[N]).
tailpr([node(N,empty,empty)|L1],[N|L]):-tailpr(L1,L).
tailpr([node(N,LBT,empty)|L1],[N|L]) :-append([LBT], L1 ,L2),
                                       tailpr(L2,L).
tailpr([node(N,empty,RBT)|L1],[N|L]) :-append([RBT], L1, L2), 
                                        tailpr(L2,L).                            
tailpr([node(N,LBT,RBT)|L1],[N|L]) :- append([LBT,RBT], L1, L2),
                                     tailpr(L2,L).


trInorder(empty,[]).
trInorder(BT, L) :- append([BT],[],L1),
                    tailin(L1,L).

tailin([],[]).                                  %implements tail recursion
tailin([node(N,1,1)|L1],[N|L]):- tailin(L1,L).
tailin([node(N,empty,empty)],[N]).
tailin([node(N,empty,empty)|L1],[N|L]):-tailin(L1,L).
tailin([node(N,LBT,empty)|L1],L) :-append([LBT,node(N,1,1)], L1 ,L2),
                                    tailin(L2,L).
tailin([node(N,empty,RBT)|L1],L) :-append([node(N,1,1),RBT], L1, L2), 
                                    tailin(L2,L).                            
tailin([node(N,LBT,RBT)|L1],L) :- append([LBT,node(N,1,1),RBT], L1, L2),
                                    tailin(L2,L).

trPostorder(empty,[]).
trPostorder(BT,L) :- append([BT],[],L1),
                     tailpo(L1,L).

tailpo([],[]).                                   %implements tail recursion
tailpo([node(N,1,1)|L1],[N|L]):- tailpo(L1,L).
tailpo([node(N,empty,empty)],[N]).
tailpo([node(N,empty,empty)|L1],[N|L]):-tailpo(L1,L).
tailpo([node(N,LBT,empty)|L1],L) :-append([LBT,node(N,1,1)], L1 ,L2),
                                    tailpo(L2,L).
tailpo([node(N,empty,RBT)|L1],L) :-append([RBT,node(N,1,1)], L1, L2), 
                                    tailpo(L2,L).                            
tailpo([node(N,LBT,RBT)|L1],L) :- append([LBT,RBT,node(N,1,1)], L1, L2),
                                    tailpo(L2,L).


eulerTour(empty,[]).
eulerTour(node(N,LBT,RBT),[N|L]) :- eulerTour(LBT,LT),
                                    eulerTour(RBT,RT),
                                    append(LT,[N|RT],L1),
                                    append(L1,[N],L).


preET(empty,[]).
preET(BT,L) :-  eulerTour(BT,L1), 
                choose1(L1,L).

inET(empty,[]).
inET(BT,L) :-   eulerTour(BT,L1),
                choose1(L1,L2),
                rem_first(L2,L1,L3),
                choose1(L3,L).

rem_first([],[],[]).         %removes first occurance of all elements. eg [1,2,2,3,3,1,3,2,1] -> [2,3,1,3,2,1]
rem_first([],[Y|L2],[Y|L]) :- rem_first([],L2,L).
rem_first([X|L1],[X|L2],L) :- rem_first(L1,L2,L).
rem_first([X|L1],[Y|L2],[Y|L]) :- rem_first([X|L1],L2,L).

postET(empty,[]).
postET(BT,L) :- eulerTour(BT,L1),
                reverseList(L1,L2),
                choose1(L2,L3),
                reverseList(L3,L).

choose1([],[]).         %makes a list by picking the first occurance of elements in given list
choose1([N|L1],L) :- rem(N,L1,L2),
                     choose1(L2,L3),
                     append([N],L3,L).

rem(_,[],[]).            %remove all X from list
rem(X,[X|Y],P) :- rem(X, Y, P).
rem(X,[Y|Z],[Y|P]) :-rem(X,Z,P).

reverseList([],[]).      %reverse a list
reverseList([N|L1],L) :- reverseList(L1,L2), append(L2,[N],L).


toString(empty,"()").
toString(node(N,LBT,RBT), S) :- A='(',B=')', Sp=', ',
                                toString(LBT,LS),
                                toString(RBT,RS),
                                string_concat(A, N, C),
                                string_concat(C, Sp, C1),
                                string_concat(C1,LS,D),
                                string_concat(D, Sp, D1),
                                string_concat(D1,RS,E),
                                string_concat(E,B,S).


isBalanced(empty).
isBalanced(node(_,LBT,RBT)) :-  height(LBT, LT),
                                height(RBT, RT),
                                RT1 is RT+1, LT1 is LT+1,
                                RT =< LT1, LT =< RT1.


isBST(empty).
isBST(BT) :- inorder(BT,L),
             check(L).

check([]).                       % check if list is sorted
check([_]).
check([X,Y|T]) :- X=<Y, check([Y|T]).
              

makeBST([],empty).
makeBST(L,BST) :- length(L,N),
                  makeBST(N,L,[],BST).       % N will denote the size of list
            
makeBST(1,[N|L],L,node(N,empty,empty)).    
makeBST(2,[N,N1|L],L,node(N1,node(N,empty,empty),empty)).
makeBST(N,L1,L2,node(X,BST1,BST2)) :- N1 is (N-1) div 2,
                                      N2 is N div 2,
                                      makeBST(N2, L1, [X|L3], BST1),
                                      makeBST(N1, L3, L2, BST2).
      
                        
lookup(N,node(M,LBT,RBT)) :- M=N; lookup(N,LBT) ; lookup(N,RBT).


insert(N,BST1,BST1) :- lookup(N,BST1).
insert(N,BST1,BST2) :- inorder(BST1,L1),
                       ins(N,L1,L2), print(L2),
                       makeBST(L2,BST2).

ins(X,[],[X]).                          %insert element in sorted list
ins(X,[Y|L],[X,Y|L]) :- X<Y.
ins(X,[Y|L1],[Y|L]) :- ins(X,L1,L).
                        

delete(N,BST1,BST2) :- inorder(BST1,L),
                       del(N,L,L1),
                       makeBST(L1,BST2).

del(_,[],[]).                          %del element from list
del(X,[X|L1],L) :- del(X,L1,L2),
                   append(L2,[],L).
del(X,[Y|L1],L) :- del(X,L1,L2),
                   append([Y],L2,L).