struct MyQueue {
    stack: Vec<i32>,
}

impl MyQueue {
    fn new() -> Self {
        Self { stack: Vec::new() }
    }

    fn push(&mut self, x: i32) {
        let mut temp = Vec::new();
        while self.stack.len() > 0 {
            temp.push(self.stack.pop().unwrap());
        }
        self.stack.push(x);
        while temp.len() > 0 {
            self.stack.push(temp.pop().unwrap());
        }
    }

    fn pop(&mut self) -> i32 {
        self.stack.pop().unwrap()
    }

    fn peek(&self) -> i32 {
        self.stack[self.stack.len() - 1]
    }

    fn empty(&self) -> bool {
        self.stack.len() == 0
    }
}
