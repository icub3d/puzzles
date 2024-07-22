use std::{
    collections::VecDeque,
    sync::{Arc, Mutex},
};

use anyhow::Result;
use tokio::sync::mpsc::{self, Receiver, Sender};

#[derive(Debug, Clone)]
pub struct ChannelSender {
    queue: Arc<Mutex<VecDeque<isize>>>,
    notifier: Sender<()>,
}

impl ChannelSender {
    fn new(queue: Arc<Mutex<VecDeque<isize>>>, notifier: Sender<()>) -> Self {
        Self { queue, notifier }
    }

    pub async fn send(&mut self, value: isize) -> Result<()> {
        self.queue.lock().unwrap().push_back(value);
        self.notifier.send(()).await?;
        Ok(())
    }
}

pub struct ChannelReceiver {
    queue: Arc<Mutex<VecDeque<isize>>>,
    notifier: Receiver<()>,
    block_on_recv: bool,
}

impl ChannelReceiver {
    fn new(
        queue: Arc<Mutex<VecDeque<isize>>>,
        notifier: Receiver<()>,
        block_on_recv: bool,
    ) -> Self {
        Self {
            queue,
            notifier,
            block_on_recv,
        }
    }

    pub async fn recv(&mut self) -> Option<isize> {
        match self.block_on_recv {
            true => match self.notifier.recv().await {
                Some(_) => {
                    let value = {
                        let data = self.queue.lock().unwrap();
                        *data.front().unwrap()
                    };
                    self.queue.lock().unwrap().pop_front();
                    Some(value)
                }
                None => None,
            },
            false => match self.notifier.try_recv() {
                Ok(_) => {
                    let value = {
                        let data = self.queue.lock().unwrap();
                        *data.front().unwrap()
                    };
                    self.queue.lock().unwrap().pop_front();
                    Some(value)
                }
                Err(_) => None,
            },
        }
    }
}

pub struct Channel {
    queue: Arc<Mutex<VecDeque<isize>>>,
}

impl Channel {
    pub fn new(block_on_recv: bool) -> (Self, ChannelSender, ChannelReceiver) {
        let (notifier_send, notifier_recv) = mpsc::channel(32);
        let queue = Arc::new(Mutex::new(VecDeque::new()));
        let sender = ChannelSender::new(queue.clone(), notifier_send);
        let receiver = ChannelReceiver::new(queue.clone(), notifier_recv, block_on_recv);

        (Self { queue }, sender, receiver)
    }

    pub fn queue(&self) -> Vec<isize> {
        self.queue.lock().unwrap().iter().copied().collect()
    }
}
