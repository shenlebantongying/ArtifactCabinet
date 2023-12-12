## Why Q_OBJECT is needed?

Limitation of most OOP systems: Hierarchies between classes are generally predefined, which prevents the adding of behaviours to objects that have not been specifically designed and implemented to receive them, both statically (at compilation) or dynamically (at runtime). Sometimes, new behaviours must be added through the base class.

Qt's solution -> moc -> compile time reflection
