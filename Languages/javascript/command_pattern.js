var carManager = {
    // request information
    requestInfo: function( model, id ){
        return "The information for " + model + " with ID " + id + " is foobar";
    },
    // arrange a viewing
    arrangeViewing: function( model, id ){
        return "You have successfully booked a viewing of " + model + " ( " + id + " ) ";
    }
};

carManager.execute = function ( name ) {
    return carManager[name].apply( carManager,              // Mandatory "this",
                                    Array.from(arguments)); // Convert arguments to simple array

};

console.log(carManager.execute( "arrangeViewing", "Ferrari", "14523" ));
console.log(carManager.execute( "requestInfo", "Ford Mondeo", "54323" ));
