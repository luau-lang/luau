type MyObject = { a: string, b: number, c: boolean }
type Missing = index<MyObject, "d">
type Invalid = index<MyObject, boolean>
