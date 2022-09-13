// NOTES on codegen
//
// - Scope is typically traced by a list of hash tables. With each hash table
//   representing the variables available at that scope. The deeper into the hash
//   table, the deeper the scope. So if there is a variable defined at the third
//   hash table and called in the fourth, that is fine. But the opposite is not
//   permitted.
//
//
