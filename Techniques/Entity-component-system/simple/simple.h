#pragma once

#include <string>
#include <vector>

// ---------------------------
// Mini ECS

class Entity;

class Component
{
public:
  Component ();

  virtual ~Component ();

  virtual void Init ();
  virtual void Update (double time, float deltatime){};

  const Entity &
  GetEntity () const
  {
    return *m_Entity;
  };

  void
  SetEntity (Entity &go)
  {
    m_Entity = &go;
  };

private:
  Entity *m_Entity;
};

// An entity is an aggregate of components
class Entity
{
public:
  explicit Entity (const std::string &&name) : m_Name (name) {}
  Entity ()
  {
    for (auto c : m_Components)
      {
        delete c;
      }
  }

  // Get Specific Component
  template <class T>
  [[nodiscard]] const T *
  GetComponent () const
  {
    for (auto i : m_Components)
      {
        // note: dynamic_cast return null when casting fails
        T *c = dynamic_cast<T *> (i);
        if (c != nullptr)
          {
            return c;
          }
      }
    return nullptr;
  }

  void
  AddComponent (Component *c)
  {
    c->SetEntity (*this);
    m_Components.emplace_back (c);
  };

private:
  std::string m_Name;
  std::vector<Component *> m_Components;
};

template <typename T>
static std::vector<Component *>
FindAllComponentsOfType (std::vector<Entity *> &s_Objects)
{
  std::vector<Component> res;
  for (auto go : s_Objects)
    {
      T *c = go->GetComponent<T> ();
      if (c != nullptr)
        {
          res.emplace_back (c);
        }
    }
}

// ---------------------------

struct MoveComponent : public Component
{
  float vx, vy;
};

struct PositionComponent : public Component
{
  float x, y;

  void
  Update (double time, float deltatime)
  {
    // TODO: is it fast here?
    auto mc = GetEntity ().GetComponent<MoveComponent> ();
    x += mc->vx;
    y += mc->vy;
  }
};

using EntityID = size_t;

struct Entities
{
  // EntityID -> index of each component
  std::vector<std::string> m_Names;
  std::vector<PositionComponent> m_Positions;
  std::vector<MoveComponent> m_Moves;

  void
  resize (size_t n)
  {
    m_Names.reserve (n);
    m_Positions.reserve (n);
    m_Moves.reserve (n);
  }

  EntityID
  addEntity (const std::string &&name)
  {
    EntityID id = m_Names.size ();
    m_Names.emplace_back (name);
    m_Positions.emplace_back ();
    m_Moves.emplace_back ();
    return id;
  }
};

struct world
{
  static void
  updatePositions (Entities *entities)
  {
    for (int i = 0; i < entities->m_Names.size (); ++i)
      {
        entities->m_Positions[i].Update (0, 0);
      }
  }
};
