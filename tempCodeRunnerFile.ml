let top_env = [
    Binding ("+", PrimV "+");
    Binding ("-", PrimV "-");
    Binding ("*", PrimV "*");
    Binding ("/", PrimV "/");
    Binding ("<=", PrimV "<=");
    Binding ("equal?", PrimV "equal?");
    Binding ("true", BooleanV true);
    Binding ("false", BooleanV false);
    Binding ("error", PrimV "error");
  ]