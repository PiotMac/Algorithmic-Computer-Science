package main

import (
	"fmt"
	"sync"
	"time"
)

const (
	numPhilosophers = 10
	mealsPerPhilosopher = 2
	timeFactor = 500
)

type Fork struct{ sync.Mutex }

type Philosopher struct {
	id                    int
	leftFork, rightFork   *Fork
	mealsEaten            int
}

func (p *Philosopher) eat(wg *sync.WaitGroup, sem *Semaphore) {
	defer wg.Done()
	for p.mealsEaten < mealsPerPhilosopher {
		fmt.Printf("Philosopher[%d] --> THINKING . . .\n", p.id)
		time.Sleep(time.Millisecond * timeFactor)

		fmt.Printf("Philosopher[%d] --> HUNGRY!\n", p.id)
		sem.Acquire()

		p.leftFork.Lock()
		p.rightFork.Lock()

		fmt.Printf("Philosopher[%d] --> EATING . . .\n", p.id)
		p.mealsEaten++

		time.Sleep(time.Millisecond * timeFactor)

		p.leftFork.Unlock()
		p.rightFork.Unlock()
		sem.Release()

		fmt.Printf("Philosopher[%d] --> MEALS YET TO EAT: %d\n", p.id, mealsPerPhilosopher - p.mealsEaten)
	}
	fmt.Printf("Philosopher[%d] --> LEAVES!\n", p.id)
}

type Semaphore chan struct{}

func NewSemaphore(n int) *Semaphore {
	sem := make(Semaphore, n)
	for i := 0; i < n; i++ {
		sem <- struct{}{}
	}
	return &sem
}

func (s *Semaphore) Acquire() {
	<-*s
}

func (s *Semaphore) Release() {
	*s <- struct{}{}
}

func main() {
	forks := make([]*Fork, numPhilosophers)

	for i := 0; i < numPhilosophers; i++ {
		forks[i] = new(Fork)
	}

	sem := NewSemaphore(numPhilosophers - 1)

	philosophers := make([]*Philosopher, numPhilosophers)
	for i := 0; i < numPhilosophers; i++ {
		philosophers[i] = &Philosopher {
			id:          i + 1,
			leftFork:    forks[i],
			rightFork:   forks[(i+1)%numPhilosophers],
			mealsEaten:  0,
		}
	}

	var wg sync.WaitGroup
	for _, p := range philosophers {
		wg.Add(1)
		go p.eat(&wg, sem)
	}

	wg.Wait()
}