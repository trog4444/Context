### Context

Types that 'enhance' generic types with context specific to the given type.
For example, the 'Maybe' type adds the context of 'nullability' to a generic type. Most types are ports of Haskell types into F#.

Each type exists in its own namespace: Context.Type."TypeName", and includes modules for 'Primitive' functions, 'Conversion' functions (as applicable), and 'Composition' functions. The Composition module(s) can include functions related to typeclasses the given type may implement (eg Functors, Monads, Monoids, Arrows, etc).

All namespaces have the following structure:

namespace Context.Type."TypeName"

type "TypeName"<...> = ...

module Primitive = ...

(as applicable)
module Convert = ...

module Composition =

  -- Functor, Applicative Functor, and Monad (minimal) functions are placed at this level (map, ap, bind, etc) if applicable --
  
  module Monad = ...
  
    -- include Workflow builder class here --
    
  -- create instance of Workflow builder (if it exists) here named as the type name (in camel-case) --
  
  module Applicative = ...
  
  etc
  
-- Operators go here --
