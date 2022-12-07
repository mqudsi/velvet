use std::collections::VecDeque;

pub(crate) struct PeekableQueue<I: Iterator> {
    queue: VecDeque<I::Item>,
    iter: I,
}

impl<I> PeekableQueue<I>
where
    I: Iterator,
{
    pub fn new(iter: I) -> Self {
        PeekableQueue {
            queue: VecDeque::new(),
            iter,
        }
    }

    pub fn peek(&mut self, index: usize) -> Option<&I::Item> {
        while self.queue.len() <= index {
            if let Some(item) = self.iter.next() {
                self.queue.push_back(item);
            } else {
                return None;
            }
        }

        self.queue.get(index)
    }

    pub fn map_while<P, B>(&mut self, pred: P) -> MapWhile<'_, I, P, B>
    where
        P: FnMut(&I::Item) -> Option<B>,
    {
        self.map_while2(pred)
    }

    pub fn map_while2<P, B>(&mut self, pred: P) -> MapWhile<'_, I, P, B>
    where
        P: FnMut(&I::Item) -> Option<B>,
    {
        MapWhile {
            peekable: self,
            pred,
        }
    }

    pub fn next_if<P>(&mut self, mut pred: P) -> Option<I::Item>
    where
        P: FnMut(&I::Item) -> bool,
    {
        match self.next() {
            Some(item) => {
                if pred(&item) {
                    Some(item)
                } else {
                    self.queue.push_front(item);
                    None
                }
            }
            None => None,
        }
    }
}

impl<I> Iterator for PeekableQueue<I>
where
    I: Iterator,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(item) = self.queue.pop_front() {
            return Some(item);
        }

        self.iter.next()
    }
}

pub(crate) trait Peekable<I>
where
    I: Iterator,
{
    fn into_peekable(self) -> PeekableQueue<I>;
}

impl<I: Iterator> Peekable<I> for I {
    fn into_peekable(self) -> PeekableQueue<I> {
        PeekableQueue::new(self)
    }
}

pub(crate) struct MapWhile<'a, I, P, B>
where
    I: Iterator + 'a,
    P: FnMut(&I::Item) -> Option<B>,
{
    peekable: &'a mut PeekableQueue<I>,
    pred: P,
}

impl<'a, I, P, B> Iterator for MapWhile<'a, I, P, B>
where
    I: Iterator + 'a,
    P: FnMut(&I::Item) -> Option<B>,
{
    type Item = B;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(item) = self.peekable.queue.pop_front() {
            match (self.pred)(&item) {
                Some(result) => return Some(result),
                None => {
                    self.peekable.queue.push_front(item);
                    return None;
                }
            }
        }

        while let Some(item) = self.peekable.iter.next() {
            match (self.pred)(&item) {
                Some(result) => return Some(result),
                None => {
                    self.peekable.queue.push_back(item);
                    return None;
                }
            }
        }

        None
    }
}
