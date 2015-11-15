(function($) {

	// Check if already liked
	check_like_status();

	var request;
	var ip_location;

	function fill_like_button(){
		$("#like_button>i").removeClass("fa-heart-o").addClass("fa-heart");
		$("#like_button").attr("title", "Thank you for HEART ME :)").off("click");
	}

	function like_button_post(){
		var formData = new FormData();

		console.log(ip_address);
		console.log(ip_location);

		formData.append("ip", ip_address); 

		formData.append("location", ip_location);

		request = $.ajax({
	        url: "https://script.google.com/macros/s/AKfycbxalMPo4hmroq27U8fPJwSwg0QECisWWoWrz42Loee4Jd0udE0/exec",
	        type: "post",
	        data: formData,
	        processData: false,
	        contentType: false
	    });

	    // callback handler that will be called on success
	    request.done(function (response, textStatus, jqXHR){
	        // log a message to the console
	        console.log("Hooray, it worked!");
	    });

	    // callback handler that will be called on failure
	    request.fail(function (jqXHR, textStatus, errorThrown){
	        // log the error to the console
	        console.error(
	            "The following error occured: "+
	            textStatus, errorThrown
	        );
	    });
	}

	function check_like_status(){
		var formData = new FormData();
		formData.append("check",true);
		formData.append("ip",ip_address);
		request = $.ajax({
	        url: "https://script.google.com/macros/s/AKfycbxalMPo4hmroq27U8fPJwSwg0QECisWWoWrz42Loee4Jd0udE0/exec",
	        type: "post",
	        data: formData,
	        processData: false,
	        contentType: false
	    });

	    // callback handler that will be called on success
	    request.done(function (response, textStatus, jqXHR){
	        // log a message to the console
	        console.log(response);
	        // logic
	        if(response == 'true'){
	        	fill_like_button();
	        }else{
	        	// Register like button action
				$("#like_button").click(function(event){
					fill_like_button();
					like_button_post();
					event.preventDefault();
				});

				//Get IP Geo Location
				$.get("http://ip-api.com/json/" + ip_address, function(data){
					if(data.status == "success"){
						ip_location = data.city + ", " + data.country;
						console.log(ip_location);
					}
					else{
						console.log(data);
					}
				});
	        }
	    });

	    // callback handler that will be called on failure
	    request.fail(function (jqXHR, textStatus, errorThrown){
	        // log the error to the console
	        console.error(
	            "The following error occured: "+
	            textStatus, errorThrown
	        );
	    });
	}
})(jQuery);