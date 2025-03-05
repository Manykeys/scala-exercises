// Тайпклассы можно написать не только лишь в Scala
// Вот, например, тайпкласс на С++
// (т.е. можно написать на любом языке, который поддерживает дженерики)

#include <iostream>
#include <cmath>

// Сущности, представляющие данные
struct Circle {
    double radius;
};

struct Rectangle {
    double width, length;
};

// Тайпкласс (шаблонный интерфейс)
template <typename T>
struct Area {
    double area(const T& shape) const;
};

// Реализация для Circle
template <>
struct Area<Circle> {
    double area(const Circle& circle) const {
        return M_PI * std::pow(circle.radius, 2);
    }
};

// Реализация для Rectangle
template <>
struct Area<Rectangle> {
    double area(const Rectangle& rectangle) const {
        return rectangle.width * rectangle.length;
    }
};

// Обобщенная функция
template <typename T>
double areaOf(const T& shape, const Area<T>& areaInstance) {
    return areaInstance.area(shape);
}

// Использование
int main() {
    Circle circle{5.0};
    Rectangle rectangle{4.0, 6.0};

    Area<Circle> circleArea;
    Area<Rectangle> rectangleArea;

    std::cout << "Area of Circle: " << areaOf(circle, circleArea) << std::endl;
    std::cout << "Area of Rectangle: " << areaOf(rectangle, rectangleArea) << std::endl;

    return 0;
}
