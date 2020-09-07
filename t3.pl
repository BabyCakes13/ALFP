% initMyData(+NBoxesToPick, -ProgramDataInitial).
% Here, you have to choose and initialise  a structure ProgramData
% Intuition:
%   ProgramData represents the "agenda" where the robotul keeps track of
%     - where it is,
%     - what is his state (does it hold a box or not), and
%     - what it has to do
%

% initMyData(+NBoxesToPick, (CurrentPosition,NBoxesToPick,LastVisitedPosition,HasBox)).
% ...

% perform(+ProgramData, +ContainsBox, -Action, -ProgramDataUpdated)
% ...

%
% Set up the rules for the robot.
% We have an array as: [x, y, boxLeft, holdsBox, previousX, previousY. goBack]
% x, y - current coordinates
% boxLeft - number of boxes left to be pciked up.
% previousX, previousY - the position of the last picked box (we need to remember them in order to
% be able to get back to them).
% holdsBox - holds a box.
% goBack - flag representing the case when the robot delivered the box and has to go back to the
% previous location.
% The robot needs to pick up the box, remember where he picked it up from, deliver it, then come back to 
% where he picked it up from.
%



hasNoBox(ProgramData):- ProgramData = [_, _, _, false, _, _, _].
hasBox(ProgramData):- ProgramData = [_, _, _, true, _, _, _].
goBackToLastLocation(ProgramData):- ProgramData = [_, _, _, _, _, _, true].
isFinished(ProgramData):- ProgramData = [_, _, N, _, _, _, _], N = 0.
isOrigin(ProgramData):- ProgramData = [x, y, _, _, _, _, _], x = 0, y = 0.



%
% When the robot finds a box, we need to update the status information: hasBox set to true and goBack set to true.
% We save the position of the found box such that we can go back where we found it and not start again all over.
% We set the hasBox to true because the robot has the box now, and it will be true until it delivers it.
%
 
findBoxStatusUpdate(ProgramData, ProgramDataUpdated):- 
   ProgramData - [T1, T2, T3, false, _, _, _], ProgramDataUpdated = [T1, T2, T3, true, T1, T2, true].



%
% When the robot delivers the box, we need to notify that the number of boxes which remain to be picked has decremented by 1, and that the robot has no box anymore.
%

deliveredBoxStatusUpdate(ProgramData, ProgramDataUpdated):-
    ProgramData = [T1, T2, T3, _, T4, T5, T6], NT3 is T3 - 1, ProgramDataUpdated = [T1, T2, NT3, false, T4, T5, T6]
    .



% 
% If the robot picked up the bos (hasBox is true), we need to deliver it to the location of the origin.
% Based on how x and y are compared to 0, we move east, west, north or south.
%
 
updateMoveWithBox(ProgramData, ProgramDataUpdated, Action):- 
    ProgramData = [x, y, boxLeft, holdsBox, previousX, previousY, goBack] , x < 0 -> (newX is x + 1, ProgramDataUpdated = [newX, y, boxLeft, holdsBox, previousX, previousY, goBack], Action = moveWithBox(east));
    ProgramData = [x, y, boxLeft, holdsBox, previousX, previousY, goBack] , x > 0 -> (newX is x - 1, ProgramDataUpdated = [newX, y, boxLeft, holdsBox, previousX, previousY, goBack], Action = moveWithBox(west));
    ProgramData = [x, y, boxLeft, holdsBox, previousX, previousY, goBack] , y < 0 -> (newY is y + 1, ProgramDataUpdated = [x, newY, boxLeft, holdsBox, previousX, previousY, goBack], Action = moveWithBox(north));
    ProgramData = [x, y, boxLeft, holdsBox, previousX, previousY, goBack] , y > 0 -> (newY is y - 1, ProgramDataUpdated = [x, newY, boxLeft, holdsBox, previousX, previousY, goBack], Action = moveWithBox(south)).


              
%
% * After the box has been delivered, we have to move the robot back from the origin to where it found the box. 
% * If the position where the box was picked up from was reached, we check the goBack flag as false, else, we move back.
% * We move one positions one by one until x = previousX and y = previousY.
%

updateToLastLocation(ProgramData, ProgramDataUpdated, Action):-
    ProgramData = [x, y, boxLeft, holdsBox, previousX, previousY, _], x = previousX, y = previousY -> (ProgramDataNew = [x, y, boxLeft, holdsBox, previousX, previousY, false], update_move(ProgramDataNew, ProgramDataUpdated, Action));
    ProgramData = [x, y, boxLeft, holdsBox, previousX, previousY, goBack], x < previousX -> (newX is x + 1, ProgramDataUpdated = [newX, y, boxLeft, holdsBox, previousX, previousY, goBack], Action = move(east));
    ProgramData = [x, y, boxLeft, holdsBox, previousX, previousY, goBack], x > previousX -> (newX is x - 1, ProgramDataUpdated = [newX, y, boxLeft, holdsBox, previousX, previousY, goBack], Action = move(west));
    ProgramData = [x, y, boxLeft, holdsBox, previousX, previousY, goBack], y < previousY -> (newY is y + 1, ProgramDataUpdated = [x, newY, boxLeft, holdsBox, previousX, previousY, goBack], Action = move(north));
    ProgramData = [x, y, boxLeft, holdsBox, previousX, previousY, goBack], y > previousY -> (newY is y - 1, ProgramDataUpdated = [x, newY, boxLeft, holdsBox, previousX, previousY, goBack], Action = move(south));


   
%
% Since the space is infinite, I believe that the best approach to the movement pattern should be in concentric circles with the middle as the origin point. Going all far to any direction cannot be done. 
% In order to change the circle after we are at the right most corner, the robot needs to move one step to the right in order to get to the outer circle of rooms, and not stay in the same circle and get stuck in a loop. From origin, I chose to take the first step right. When in circle, we have four possible places (the four neighbours: east, west, south and north, aka the first, second, third and fourth quadrants) Based on that, we will determine the quadrant we are in, and move accordingly.
%
 
updateMove(ProgramData, ProgramDataUpdated, Action):-
    % Make the first move to the right from the origin.
    ProgramData = [x, y, boxLeft, holdsBox, previousX, previousY, goBack], x = 0, y = 0 -> (newX is x + 1, ProgramDataUpdated = [newX, y, boxLeft, holdsBox, previousX, previousY, goBack], Action = move(east));
    
    % First quadrant. 
    ProgramData = [x, y, boxLeft, holdsBox, previousX, previousY, goBack], x > 0, y >= 0, maximum is max(x,y), x = maximum, y\= maximum -> (newY is y + 1, ProgramDataUpdated = [x, newY, boxLeft, holdsBox, previousX, previousY, goBack], Action = move(north));
    ProgramData = [x, y, boxLeft, holdsBox, previousX, previousY, goBack], x > 0, y > 0, maximum is max(x,y), x = maximum, y = maximum -> (newX is x - 1, ProgramDataUpdated = [newX, y, boxLeft, holdsBox, previousX, previousY, goBack], Action = move(west));
    ProgramData = [x, y, boxLeft, holdsBox, previousX, previousY, goBack], x > 0, y > 0, maximum is max(x,y), x = \maximum, y = maximum -> (newX is x - 1, ProgramDataUpdated = [newX, y, boxLeft, holdsBox, previousX, previousY, goBack], Action = move(west));
    
     % Second quadrant.
    ProgramData = [x, y, boxLeft, holdsBox, previousX, previousY, goBack], x < 0, y > 0, absX is abs(x), maximum is max(absX,y), absX = maximum, y = maximum -> (newY is y - 1, ProgramDataUpdated = [x, newY, boxLeft, holdsBox, previousX, previousY, goBack], Action = move(south));
    ProgramData = [x, y, boxLeft, holdsBox, previousX, previousY, goBack], x < 0, y > 0,  absX is abs(x), maximum is max(absX,y), absX = maximum, y = \maximum -> (newY is y - 1, ProgramDataUpdated = [x, newY, boxLeft, holdsBox, previousX, previousY, goBack], Action = move(south));
    ProgramData = [x, y, boxLeft, holdsBox, previousX, previousY, goBack], x <= 0, y > 0,  absX is abs(x), maximum is max(absX,y), absX = \maximum, y = maximum -> (newX is x - 1, ProgramDataUpdated = [newX, y, boxLeft, holdsBox, previousX, previousY, goBack], Action = move(west));
    
    % Third quadrant.
    ProgramData = [x, y, boxLeft, holdsBox, previousX, previousY, goBack], x < 0, y < 0, absX is abs(x), absY is abs(y), maximum is max(absX,absY), absX = maximum, absY = maximum -> (newX is x + 1, ProgramDataUpdated = [newX, y, boxLeft, holdsBox, previousX, previousY, goBack], Action = move(east));
    ProgramData = [x, y, boxLeft, holdsBox, previousX, previousY, goBack], x < 0, y < 0, absX is abs(x), absY is abs(y), maximum is max(absX,absY), absX \= maximum, absY = maximum -> (newX is x + 1, ProgramDataUpdated = [newX, y, boxLeft, holdsBox, previousX, previousY, goBack], Action = move(east));
    ProgramData = [x, y, boxLeft, holdsBox, previousX, previousY, goBack], x < 0, y =< 0, absX is abs(x), absY is abs(y), maximum is max(absX,absY), absX = maximum, absY \= maximum -> (newY is y - 1, ProgramDataUpdated = [x newY, boxLeft, holdsBox, previousX, previousY, goBack], Action = move(south));
    
    % Fourth quadrant.
    ProgramData = [x, y, boxLeft, holdsBox, previousX, previousY, goBack], x > 0, y < 0, absX is abs(x), absY is abs(y), maximum is max(absX,absY), absX = maximum, absY = maximum -> (newX is x + 1, ProgramDataUpdated = [newX, y, boxLeft, holdsBox, previousX, previousY, goBack], Action = move(east));
    ProgramData = [x, y, boxLeft, holdsBox, previousX, previousY, goBack], x > 0, y < 0, absX is abs(x), absY is abs(y), maximum is max(absX,absY), absX = maximum, absY \= maximum -> (newY is y + 1, ProgramDataUpdated = [x, newY, boxLeft, holdsBox, previousX, previousY, goBack], Action = move(north));
    ProgramData = [x, y, boxLeft, holdsBox, previousX, previousY, goBack], x >= 0, y < 0, absX is abs(x), absY is abs(y), maximum is max(absX,absY), absX \= maximum, absY = maximum -> (newX is x + 1, ProgramDataUpdated = [newX, y, boxLeft, holdsBox, previousX, previousY, goBack], Action = move(east))
    .


% run
perform(ProgramData, ContainsBox, Action, ProgramDataUpdated):-
    isFinished(ProgramData) -> (ProgramDataUpdated = ProgramData, Action = done);
    ContainsBox = true, hasNoBox(ProgramData) -> ( findBoxStatusUpdate(ProgramData, ProgramDataUpdated), Action = pickBox ); 
    isOrigin(ProgramData), hasBox(ProgramData) -> ( deliveredBoxStatusUpdate(ProgramData,ProgramDataUpdated), Action = deliverBox ); 
    hasBox(ProgramData) -> ( updateMoveWithBox(ProgramData, ProgramDataUpdated, Action));
    hasNoBox(ProgramData), ( goBackToLastLocation(ProgramData) -> ( updateToLastLocation(ProgramData, ProgramDataUpdated, Action) );
    hasBox(ProgramData) -> (updateMove(ProgramData, ProgramDataUpdated, Action))
    .


    
    
