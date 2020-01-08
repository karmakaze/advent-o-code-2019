package main

import (
	"fmt"
)

// Vector //
type Vector struct {
	x int32
	y int32
	z int32
}

func (v Vector) mag() int32 {
	var absX, absY, absZ int32

	if v.x >= 0 {
		absX = v.x
	} else {
		absX = -v.x
	}
	if v.y >= 0 {
		absY = v.y
	} else {
		absY = -v.y
	}
	if v.z >= 0 {
		absZ = v.z
	} else {
		absZ = -v.z
	}

	return absX + absY + absZ
}

func (v *Vector) add(o Vector) {
	v.x += o.x
	v.y += o.y
	v.z += o.z
}

func (v *Vector) sub(o Vector) {
	v.x -= o.x
	v.y -= o.y
	v.z -= o.z
}

// Moon //
type Moon struct {
	position Vector
	velocity Vector
}

// Moons //
type Moons struct {
	moons [4]Moon
	time  int
}

func (m *Moon) applyGravity(gravity Vector) {
	m.velocity.x += gravity.x
	m.velocity.y += gravity.y
	m.velocity.z += gravity.z
}

func (m *Moon) applyVelocity() {
	m.position.x += m.velocity.x
	m.position.y += m.velocity.y
	m.position.z += m.velocity.z
}

func (m *Moon) energy() int32 {
	potential := m.position.mag()
	kinetic := m.velocity.mag()
	return potential * kinetic
}

func (ms *Moons) setMoon(i int, x, y, z int32) {
	ms.moons[i] = Moon{
		position: Vector{x, y, z},
		velocity: Vector{0, 0, 0},
	}
}

func (ms *Moons) equalsX(os *Moons) bool {
	for i := 0; i < 4; i++ {
		if ms.moons[i].position.x != os.moons[i].position.x ||
			ms.moons[i].velocity.x != os.moons[i].velocity.x {
			return false
		}
	}
	return true
}

func (ms *Moons) equalsY(os *Moons) bool {
	for i := 0; i < 4; i++ {
		if ms.moons[i].position.y != os.moons[i].position.y ||
			ms.moons[i].velocity.y != os.moons[i].velocity.y {
			return false
		}
	}
	return true
}

func (ms *Moons) equalsZ(os *Moons) bool {
	for i := 0; i < 4; i++ {
		if ms.moons[i].position.z != os.moons[i].position.z ||
			ms.moons[i].velocity.z != os.moons[i].velocity.z {
			return false
		}
	}
	return true
}

func (ms *Moons) step() {
	gravities := make([]Vector, 4)

	for i := 1; i < 4; i++ {
		for j := 0; j < i; j++ {
			g := sgn(ms.moons[i].position, ms.moons[j].position)
			gravities[i].add(g)
			gravities[j].sub(g)
		}
	}
	for i := 0; i < 4; i++ {
		ms.moons[i].applyGravity(gravities[i])
		ms.moons[i].applyVelocity()
	}
	ms.time++
}

func (ms *Moons) energy() int32 {
	var total int32
	for i := 0; i < 4; i++ {
		total += ms.moons[i].energy()
	}
	return total
}

func sgn(v1, v2 Vector) Vector {
	v := Vector{}
	if v1.x < v2.x {
		v.x = 1
	} else if v1.x > v2.x {
		v.x = -1
	}
	if v1.y < v2.y {
		v.y = 1
	} else if v1.y > v2.y {
		v.y = -1
	}
	if v1.z < v2.z {
		v.z = 1
	} else if v1.z > v2.z {
		v.z = -1
	}
	return v
}

func gcd(a, b int) int {
	if a == 0 {
		return b
	}
	if b == 0 {
		return a
	}

	r := a % b

	return gcd(b, r)
}

func main() {
	tortoise := Moons{}

	// tortoise.setMoon(0, -1, 0, 2)
	// tortoise.setMoon(1, 2, -10, -7)
	// tortoise.setMoon(2, 4, -8, 8)
	// tortoise.setMoon(3, 3, 5, -1)

	// tortoise.setMoon(0, -8, -10, 0)
	// tortoise.setMoon(1, 5, 5, 10)
	// tortoise.setMoon(2, 2, -7, 3)
	// tortoise.setMoon(3, 9, -8, -3)

	tortoise.setMoon(0, 5, -1, 5)
	tortoise.setMoon(1, 0, -14, 2)
	tortoise.setMoon(2, 16, 4, 0)
	tortoise.setMoon(3, 18, 1, 16)

	hare := Moons{}
	hare.moons = tortoise.moons

	var periodX, periodY, periodZ int
	var phaseX, phaseY, phaseZ int

	for i := 1; true; i++ {
		tortoise.step()

		hare.step()
		hare.step()

		if periodX == 0 && tortoise.equalsX(&hare) {
			periodX = hare.time - tortoise.time
			phaseX = tortoise.time % periodX
			fmt.Printf("X: tortoise %d hare %d\n", tortoise.time, hare.time)
			if periodY != 0 && periodZ != 0 {
				break
			}
		}
		if periodY == 0 && tortoise.equalsY(&hare) {
			periodY = hare.time - tortoise.time
			phaseY = tortoise.time % periodY
			fmt.Printf("Y: tortoise %d hare %d\n", tortoise.time, hare.time)
			if periodX != 0 && periodZ != 0 {
				break
			}
		}
		if periodZ == 0 && tortoise.equalsZ(&hare) {
			periodZ = hare.time - tortoise.time
			phaseZ = tortoise.time % periodZ
			fmt.Printf("Z: tortoise %d hare %d\n", tortoise.time, hare.time)
			if periodX != 0 && periodY != 0 {
				break
			}
		}
	}
	fmt.Printf("X: phase %d period %d\n", phaseX, periodX)
	fmt.Printf("Y: phase %d period %d\n", phaseY, periodY)
	fmt.Printf("Z: phase %d period %d\n", phaseZ, periodZ)

	xy := periodX / gcd(periodX, periodY) * periodY
	xyz := xy / gcd(xy, periodZ) * periodZ

	fmt.Printf("XYZ: period %d\n", xyz)
}
