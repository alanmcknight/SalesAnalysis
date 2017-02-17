var i ;
Shiny.addCustomMessageHandler("myCallbackHandler",     
    function(number) {
     i = number;
    }
);
dataLayer = [{ 'virtualPage' : i}];