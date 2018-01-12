package chapter6

class State[S,+A](run: S => (A,S)) {

}
