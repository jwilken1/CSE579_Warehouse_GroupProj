%initialize values for location of objects
%For every init statement there is a location(Object, Value, Time) with time set to 0
location(O, V, 0) :- init(O, V).

#show location/3.