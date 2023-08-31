{
  programs.micro = {
  	enable = true;

  	settings = {
  	  tabstospaces = true;
  	  tabsize = 2;
  	};
  };
  
  xdg.configFile = {
    "micro/bindings.json".source = ../sources/micro/bindings;
  };
}
