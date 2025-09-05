# 🎨 Licensify - Art License NFT Registry

> A decentralized registry for managing art licenses as NFTs with customizable usage terms on the Stacks blockchain.

## 📋 Overview

Licensify enables artists to create, manage, and monetize their artwork through blockchain-based licensing. Each license is represented as an NFT with specific usage terms, duration, and pricing.

## ✨ Features

- 🖼️ **NFT-Based Licenses**: Each art license is a unique NFT
- 💰 **Flexible Pricing**: Set custom prices for different license types
- ⏰ **Time-Based Licensing**: Licenses with configurable duration
- 📊 **Usage Tracking**: Log and monitor license usage
- 🏷️ **Multiple License Types**: Commercial, Personal, Editorial, and Exclusive
- 📈 **Creator Analytics**: Track revenue and license statistics
- 🔄 **Transferable**: License NFTs can be transferred between users

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet for testing

### Installation

1. Clone the repository
2. Navigate to the project directory
3. Run Clarinet commands

```bash
clarinet check
```

```bash
clarinet test
```

```bash
clarinet console
```

## 📖 Usage

### Creating a License

Artists can create new art licenses by calling the `create-license` function:

```clarity
(contract-call? .Licensify create-license 
  "artwork-hash-here" 
  "My Artwork Title" 
  u1  ;; Commercial license
  "Usage terms and conditions"
  u1000000  ;; Price in microSTX
  u144  ;; Duration in blocks (~1 day)
)
```

### License Types

- `u1` - Commercial License
- `u2` - Personal License  
- `u3` - Editorial License
- `u4` - Exclusive License

### Purchasing a License

Users can purchase licenses using:

```clarity
(contract-call? .Licensify purchase-license u1)
```

### Logging Usage

Track how licenses are being used:

```clarity
(contract-call? .Licensify log-usage 
  u1 
  "website-banner" 
  "Used on company homepage"
)
```

### Managing Licenses

License owners can:
- Update pricing: `update-license-price`
- Deactivate licenses: `deactivate-license`
- Transfer ownership: `transfer-license`

## 🔍 Read-Only Functions

- `get-license` - Get license details
- `get-license-owner` - Get current owner
- `get-purchase-info` - Get purchase details
- `is-license-valid` - Check if license is still valid
- `get-creator-stats` - Get creator statistics
- `get-usage-log` - Get usage history

## 💡 Example Workflow

1. **Artist creates license** → Mints NFT with usage terms
2. **Buyer purchases license** → Pays STX, gets usage rights
3. **Buyer uses artwork** → Logs usage for transparency
4. **License expires** → Usage rights end automatically

## 🛡️ Security Features

- Owner-only functions protected
- License expiration enforcement
- Payment validation
- Usage authorization checks

## 🏗️ Contract Architecture

The contract uses several data structures:
- `licenses` - Core license information
- `license-purchases` - Purchase and expiration tracking
- `creator-stats` - Artist performance metrics
- `license-usage-log` - Usage history and compliance

## 📊 Platform Economics

- Platform fee: 2.5% (configurable by contract owner)
- Revenue split: 97.5% to creator, 2.5% to platform
- All payments in STX

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test thoroughly
5. Submit a pull request

## 📄 License

This project is open source and available under the MIT License.

---

Built with ❤️ on Stacks blockchain
```

**Git Commit Message:**
```
feat: implement# Licensify

