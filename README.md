My first foray in Purescript. Since FFI is supposed to be easy, that's
where we start. Firebase.

1. 24 days of purescript had object-ffi. Make an object-ffi that can 
 - create a new Firebase
 - push a hardcoded value to our test database /purescript/{success:
true}

2. Run above from main
3. Make that into a test (removing a value? we are not that interested
   in removing, given event sourcing. for testing it might be handy. or:
parameterize, and write success: false. Need reading. child_added /
child_modified?
