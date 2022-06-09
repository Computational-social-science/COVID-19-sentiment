import torch
from build_data import *
from transformers import BertModel
import torch.nn as nn
from tqdm import tqdm 
import torch.nn.functional as F

class Model(nn.Module):
    def __init__(self, num_classes):
        super(Model, self).__init__()
        self.bert = BertModel.from_pretrained('./chinese_wwm_ext_pytorch') 
        for param in self.bert.parameters():
            param.requires_grad = True  
        self.fc = nn.Linear(768, num_classes)   # 768 -> 6
    def forward(self, x, token_type_ids, attention_mask):
        context = x  
        types = token_type_ids
        mask = attention_mask  
        _, pooled = self.bert(context, token_type_ids=types, attention_mask=mask)
        out = self.fc(pooled)   
        return out

MODEL1 = Model(num_classes=2) 
print('The original model is loaded')
DEVICE = torch.device("cuda" if torch.cuda.is_available() else "cpu")
MODEL = MODEL1.to(DEVICE)
OPTIMIZER = torch.optim.Adam(MODEL.parameters(), lr=2e-5) # The optimizer
NUM_EPOCHS = 20 # epoch
PATH = './model/model.ckpt'  

def train(model, device, train_loader, test_loader, optimizer):   
    model.train()
    best_acc = 0.0
    for epoch in range(1, NUM_EPOCHS + 1): 
        batch_idx = 0
        for (x1, x2, x3, y) in tqdm(train_loader):
            x1, x2, x3, y = x1.to(device), x2.to(device), x3.to(device), y.to(device)
            y_pred = model(x1, token_type_ids=x2, attention_mask=x3) 
            optimizer.zero_grad()            
            loss = F.cross_entropy(y_pred, y.squeeze()) 
            
            loss.backward()
            optimizer.step()
            batch_idx += 1
            if(batch_idx + 1) % 100 == 0:   
                print('Train Epoch: {} [{}/{} ({:.2f}%)]\tLoss: {:.6f}'.format(epoch, (batch_idx+1) * len(x1),
                                                                               len(train_loader.dataset),
                                                                               100. * batch_idx / len(train_loader),
                                                                               
                                                                               loss.item())) 
        acc = test(model, device, test_loader) 
        if best_acc < acc:
            best_acc = acc
            torch.save(model.state_dict(), PATH)  # Save the optimal model

def test(model, device, test_loader):    # Test the model and get the test set evaluation results
    model.eval()
    test_loss = 0.0
    acc = 0
    for (x1, x2, x3, y) in tqdm(test_loader):
        x1, x2, x3, y = x1.to(device), x2.to(device), x3.to(device), y.to(device)
        with torch.no_grad():
            y_ = model(x1, token_type_ids=x2, attention_mask=x3)
        test_loss += F.cross_entropy(y_, y.squeeze())
        pred = y_.max(-1, keepdim=True)[1]  
        acc += pred.eq(y.view_as(pred)).sum().item()   
    test_loss /= len(test_loader)
    print('\nTest set: Average loss: {:.4f}, Accuracy: {}/{} ({:.2f}%)'.format(
          test_loss, acc, len(test_loader.dataset),
          100. * acc / len(test_loader.dataset)))
    return acc / len(test_loader.dataset)

def main():
    train_data = genDataLoader(True)
    print('The training set is processed')
    test_data = genDataLoader(False)
    print('The evaluating set is processed')
    train(MODEL, DEVICE, train_data, test_data, OPTIMIZER)
if __name__ == '__main__':
    main()
